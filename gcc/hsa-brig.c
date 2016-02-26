/* Producing binary form of HSA BRIG from our internal representation.
   Copyright (C) 2013-2016 Free Software Foundation, Inc.
   Contributed by Martin Jambor <mjambor@suse.cz> and
   Martin Liska <mliska@suse.cz>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "target.h"
#include "tm_p.h"
#include "is-a.h"
#include "vec.h"
#include "hash-table.h"
#include "hash-map.h"
#include "tree.h"
#include "tree-iterator.h"
#include "stor-layout.h"
#include "output.h"
#include "cfg.h"
#include "function.h"
#include "fold-const.h"
#include "stringpool.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "cgraph.h"
#include "dumpfile.h"
#include "print-tree.h"
#include "symbol-summary.h"
#include "hsa.h"
#include "gomp-constants.h"

/* Convert VAL to little endian form, if necessary.  */

static uint16_t
lendian16 (uint16_t val)
{
#if GCC_VERSION >= 4006
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  return val;
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  return __builtin_bswap16 (val);
#else   /* __ORDER_PDP_ENDIAN__ */
  return val;
#endif
#else
// provide a safe slower default, with shifts and masking
#ifndef WORDS_BIGENDIAN
  return val;
#else
  return (val >> 8) | (val << 8);
#endif
#endif
}

/* Convert VAL to little endian form, if necessary.  */

static uint32_t
lendian32 (uint32_t val)
{
#if GCC_VERSION >= 4006
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  return val;
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  return __builtin_bswap32 (val);
#else  /* __ORDER_PDP_ENDIAN__ */
  return (val >> 16) | (val << 16);
#endif
#else
// provide a safe slower default, with shifts and masking
#ifndef WORDS_BIGENDIAN
  return val;
#else
  val = ((val & 0xff00ff00) >> 8) | ((val & 0xff00ff) << 8);
  return (val >> 16) | (val << 16);
#endif
#endif
}

/* Convert VAL to little endian form, if necessary.  */

static uint64_t
lendian64 (uint64_t val)
{
#if GCC_VERSION >= 4006
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  return val;
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  return __builtin_bswap64 (val);
#else  /* __ORDER_PDP_ENDIAN__ */
  return (((val & 0xffffll) << 48)
	  | ((val & 0xffff0000ll) << 16)
	  | ((val & 0xffff00000000ll) >> 16)
	  | ((val & 0xffff000000000000ll) >> 48));
#endif
#else
// provide a safe slower default, with shifts and masking
#ifndef WORDS_BIGENDIAN
  return val;
#else
  val = (((val & 0xff00ff00ff00ff00ll) >> 8)
	 | ((val & 0x00ff00ff00ff00ffll) << 8));
  val = ((( val & 0xffff0000ffff0000ll) >> 16)
	 | (( val & 0x0000ffff0000ffffll) << 16));
  return (val >> 32) | (val << 32);
#endif
#endif
}

#define BRIG_ELF_SECTION_NAME ".brig"
#define BRIG_LABEL_STRING "hsa_brig"
#define BRIG_SECTION_DATA_NAME    "hsa_data"
#define BRIG_SECTION_CODE_NAME    "hsa_code"
#define BRIG_SECTION_OPERAND_NAME "hsa_operand"

#define BRIG_CHUNK_MAX_SIZE (64 * 1024)

/* Required HSA section alignment.  */

#define HSA_SECTION_ALIGNMENT 16

/* Chunks of BRIG binary data.  */

struct hsa_brig_data_chunk
{
  /* Size of the data already stored into a chunk.  */
  unsigned size;

  /* Pointer to the data.  */
  char *data;
};

/* Structure representing a BRIG section, holding and writing its data.  */

class hsa_brig_section
{
public:
  /* Section name that will be output to the BRIG.  */
  const char *section_name;
  /* Size in bytes of all data stored in the section.  */
  unsigned total_size;
  /* The size of the header of the section including padding.  */
  unsigned header_byte_count;
  /* The size of the header of the section without any padding.  */
  unsigned header_byte_delta;

  /* Buffers of binary data, each containing BRIG_CHUNK_MAX_SIZE bytes.  */
  vec <struct hsa_brig_data_chunk> chunks;

  /* More convenient access to the last chunk from the vector above.  */
  struct hsa_brig_data_chunk *cur_chunk;

  void allocate_new_chunk ();
  void init (const char *name);
  void release ();
  void output ();
  unsigned add (const void *data, unsigned len);
  void round_size_up (int factor);
  void *get_ptr_by_offset (unsigned int offset);
};

static struct hsa_brig_section brig_data, brig_code, brig_operand;
static uint32_t brig_insn_count;
static bool brig_initialized = false;

/* Mapping between emitted HSA functions and their offset in code segment.  */
static hash_map<tree, BrigCodeOffset32_t> *function_offsets;

/* Hash map of emitted function declarations.  */
static hash_map <tree, BrigDirectiveExecutable *> *emitted_declarations;

/* Hash table of emitted internal function declaration offsets.  */
hash_table <hsa_internal_fn_hasher> *hsa_emitted_internal_decls;

/* List of sbr instructions.  */
static vec <hsa_insn_sbr *> *switch_instructions;

struct function_linkage_pair
{
  function_linkage_pair (tree decl, unsigned int off)
    : function_decl (decl), offset (off) {}

  /* Declaration of called function.  */
  tree function_decl;

  /* Offset in operand section.  */
  unsigned int offset;
};

/* Vector of function calls where we need to resolve function offsets.  */
static auto_vec <function_linkage_pair> function_call_linkage;

/* Add a new chunk, allocate data for it and initialize it.  */

void
hsa_brig_section::allocate_new_chunk ()
{
  struct hsa_brig_data_chunk new_chunk;

  new_chunk.data = XCNEWVEC (char, BRIG_CHUNK_MAX_SIZE);
  new_chunk.size = 0;
  cur_chunk = chunks.safe_push (new_chunk);
}

/* Initialize the brig section.  */

void
hsa_brig_section::init (const char *name)
{
  section_name = name;
  /* While the following computation is basically wrong, because the intent
     certainly wasn't to have the first character of name and padding, which
     are a part of sizeof (BrigSectionHeader), included in the first addend,
     this is what the disassembler expects.  */
  total_size = sizeof (BrigSectionHeader) + strlen (section_name);
  chunks.create (1);
  allocate_new_chunk ();
  header_byte_delta = total_size;
  round_size_up (4);
  header_byte_count = total_size;
}

/* Free all data in the section.  */

void
hsa_brig_section::release ()
{
  for (unsigned i = 0; i < chunks.length (); i++)
    free (chunks[i].data);
  chunks.release ();
  cur_chunk = NULL;
}

/* Write the section to the output file to a section with the name given at
   initialization.  Switches the output section and does not restore it.  */

void
hsa_brig_section::output ()
{
  struct BrigSectionHeader section_header;
  char padding[8];

  section_header.byteCount = lendian64 (total_size);
  section_header.headerByteCount = lendian32 (header_byte_count);
  section_header.nameLength = lendian32 (strlen (section_name));
  assemble_string ((const char *) &section_header, 16);
  assemble_string (section_name, (section_header.nameLength));
  memset (&padding, 0, sizeof (padding));
  /* This is also a consequence of the wrong header size computation described
     in a comment in hsa_brig_section::init.  */
  assemble_string (padding, 8);
  for (unsigned i = 0; i < chunks.length (); i++)
    assemble_string (chunks[i].data, chunks[i].size);
}

/* Add to the stream LEN bytes of opaque binary DATA.  Return the offset at
   which it was stored.  */

unsigned
hsa_brig_section::add (const void *data, unsigned len)
{
  unsigned offset = total_size;

  gcc_assert (len <= BRIG_CHUNK_MAX_SIZE);
  if (cur_chunk->size > (BRIG_CHUNK_MAX_SIZE - len))
    allocate_new_chunk ();

  memcpy (cur_chunk->data + cur_chunk->size, data, len);
  cur_chunk->size += len;
  total_size += len;

  return offset;
}

/* Add padding to section so that its size is divisible by FACTOR.  */

void
hsa_brig_section::round_size_up (int factor)
{
  unsigned padding, res = total_size % factor;

  if (res == 0)
    return;

  padding = factor - res;
  total_size += padding;
  if (cur_chunk->size > (BRIG_CHUNK_MAX_SIZE - padding))
    {
      padding -= BRIG_CHUNK_MAX_SIZE - cur_chunk->size;
      cur_chunk->size = BRIG_CHUNK_MAX_SIZE;
      allocate_new_chunk ();
    }

  cur_chunk->size += padding;
}

/* Return pointer to data by global OFFSET in the section.  */

void *
hsa_brig_section::get_ptr_by_offset (unsigned int offset)
{
  gcc_assert (offset < total_size);
  offset -= header_byte_delta;

  unsigned i;
  for (i = 0; offset >= chunks[i].size; i++)
    offset -= chunks[i].size;

  return chunks[i].data + offset;
}

/* BRIG string data hashing.  */

struct brig_string_slot
{
  const char *s;
  char prefix;
  int len;
  uint32_t offset;
};

/* Hash table helpers.  */

struct brig_string_slot_hasher : pointer_hash <brig_string_slot>
{
  static inline hashval_t hash (const value_type);
  static inline bool equal (const value_type, const compare_type);
  static inline void remove (value_type);
};

/* Returns a hash code for DS.  Adapted from libiberty's htab_hash_string
   to support strings that may not end in '\0'.  */

inline hashval_t
brig_string_slot_hasher::hash (const value_type ds)
{
  hashval_t r = ds->len;
  int i;

  for (i = 0; i < ds->len; i++)
     r = r * 67 + (unsigned) ds->s[i] - 113;
  r = r * 67 + (unsigned) ds->prefix - 113;
  return r;
}

/* Returns nonzero if DS1 and DS2 are equal.  */

inline bool
brig_string_slot_hasher::equal (const value_type ds1, const compare_type ds2)
{
  if (ds1->len == ds2->len)
    return ds1->prefix == ds2->prefix
      && memcmp (ds1->s, ds2->s, ds1->len) == 0;

  return 0;
}

/* Deallocate memory for DS upon its removal.  */

inline void
brig_string_slot_hasher::remove (value_type ds)
{
  free (const_cast<char *> (ds->s));
  free (ds);
}

/* Hash for strings we output in order not to duplicate them needlessly.  */

static hash_table<brig_string_slot_hasher> *brig_string_htab;

/* Emit a null terminated string STR to the data section and return its
   offset in it.  If PREFIX is non-zero, output it just before STR too.
   Sanitize the string if SANITIZE option is set to true.  */

static unsigned
brig_emit_string (const char *str, char prefix = 0, bool sanitize = true)
{
  unsigned slen = strlen (str);
  unsigned offset, len = slen + (prefix ? 1 : 0);
  uint32_t hdr_len = lendian32 (len);
  brig_string_slot s_slot;
  brig_string_slot **slot;
  char *str2;

  str2 = xstrdup (str);

  if (sanitize)
    hsa_sanitize_name (str2);
  s_slot.s = str2;
  s_slot.len = slen;
  s_slot.prefix = prefix;
  s_slot.offset = 0;

  slot = brig_string_htab->find_slot (&s_slot, INSERT);
  if (*slot == NULL)
    {
      brig_string_slot *new_slot = XCNEW (brig_string_slot);

      /* In theory we should fill in BrigData but that would mean copying
	 the string to a buffer for no reason, so we just emulate it.  */
      offset = brig_data.add (&hdr_len, sizeof (hdr_len));
      if (prefix)
	brig_data.add (&prefix, 1);

      brig_data.add (str2, slen);
      brig_data.round_size_up (4);

      /* TODO: could use the string we just copied into
	 brig_string->cur_chunk */
      new_slot->s = str2;
      new_slot->len = slen;
      new_slot->prefix = prefix;
      new_slot->offset = offset;
      *slot = new_slot;
    }
  else
    {
      offset = (*slot)->offset;
      free (str2);
    }

  return offset;
}

/* Linked list of queued operands.  */

static struct operand_queue
{
  /* First from the chain of queued operands.  */
  hsa_op_base *first_op, *last_op;

  /* The offset at which the next operand will be enqueued.  */
  unsigned projected_size;

} op_queue;

/* Unless already initialized, initialize infrastructure to produce BRIG.  */

static void
brig_init (void)
{
  brig_insn_count = 0;

  if (brig_initialized)
    return;

  brig_string_htab = new hash_table<brig_string_slot_hasher> (37);
  brig_data.init (BRIG_SECTION_DATA_NAME);
  brig_code.init (BRIG_SECTION_CODE_NAME);
  brig_operand.init (BRIG_SECTION_OPERAND_NAME);
  brig_initialized = true;

  struct BrigDirectiveModule moddir;
  memset (&moddir, 0, sizeof (moddir));
  moddir.base.byteCount = lendian16 (sizeof (moddir));

  char *modname;
  if (main_input_filename && *main_input_filename != '\0')
    {
      const char *part = strrchr (main_input_filename, '/');
      if (!part)
	part = main_input_filename;
      else
	part++;
      modname = concat ("&__hsa_module_", part, NULL);
      char *extension = strchr (modname, '.');
      if (extension)
	*extension = '\0';

      /* As in LTO mode, we have to emit a different module names.  */
      if (flag_ltrans)
	{
	  part = strrchr (asm_file_name, '/');
	  if (!part)
	    part = asm_file_name;
	  else
	    part++;
	  char *modname2;
	  asprintf (&modname2, "%s_%s", modname, part);
	  free (modname);
	  modname = modname2;
	}

      hsa_sanitize_name (modname);
      moddir.name = brig_emit_string (modname);
      free (modname);
    }
  else
    moddir.name = brig_emit_string ("__hsa_module_unnamed", '&');
  moddir.base.kind = lendian16 (BRIG_KIND_DIRECTIVE_MODULE);
  moddir.hsailMajor = lendian32 (BRIG_VERSION_HSAIL_MAJOR);
  moddir.hsailMinor = lendian32 (BRIG_VERSION_HSAIL_MINOR);
  moddir.profile = hsa_full_profile_p () ? BRIG_PROFILE_FULL: BRIG_PROFILE_BASE;
  if (hsa_machine_large_p ())
    moddir.machineModel = BRIG_MACHINE_LARGE;
  else
    moddir.machineModel = BRIG_MACHINE_SMALL;
  moddir.defaultFloatRound = BRIG_ROUND_FLOAT_DEFAULT;
  brig_code.add (&moddir, sizeof (moddir));
}

/* Free all BRIG data.  */

static void
brig_release_data (void)
{
  delete brig_string_htab;
  brig_data.release ();
  brig_code.release ();
  brig_operand.release ();

  brig_initialized = 0;
}

/* Enqueue operation OP.  Return the offset at which it will be stored.  */

static unsigned int
enqueue_op (hsa_op_base *op)
{
  unsigned ret;

  if (op->m_brig_op_offset)
    return op->m_brig_op_offset;

  ret = op_queue.projected_size;
  op->m_brig_op_offset = op_queue.projected_size;

  if (!op_queue.first_op)
    op_queue.first_op = op;
  else
    op_queue.last_op->m_next = op;
  op_queue.last_op = op;

  if (is_a <hsa_op_immed *> (op))
    op_queue.projected_size += sizeof (struct BrigOperandConstantBytes);
  else if (is_a <hsa_op_reg *> (op))
    op_queue.projected_size += sizeof (struct BrigOperandRegister);
  else if (is_a <hsa_op_address *> (op))
    op_queue.projected_size += sizeof (struct BrigOperandAddress);
  else if (is_a <hsa_op_code_ref *> (op))
    op_queue.projected_size += sizeof (struct BrigOperandCodeRef);
  else if (is_a <hsa_op_code_list *> (op))
    op_queue.projected_size += sizeof (struct BrigOperandCodeList);
  else if (is_a <hsa_op_operand_list *> (op))
    op_queue.projected_size += sizeof (struct BrigOperandOperandList);
  else
    gcc_unreachable ();
  return ret;
}


/* Emit directive describing a symbol if it has not been emitted already.
   Return the offset of the directive.  */

static unsigned
emit_directive_variable (struct hsa_symbol *symbol)
{
  struct BrigDirectiveVariable dirvar;
  unsigned name_offset;
  static unsigned res_name_offset;

  if (symbol->m_directive_offset)
    return symbol->m_directive_offset;

  memset (&dirvar, 0, sizeof (dirvar));
  dirvar.base.byteCount = lendian16 (sizeof (dirvar));
  dirvar.base.kind = lendian16 (BRIG_KIND_DIRECTIVE_VARIABLE);
  dirvar.allocation = symbol->m_allocation;

  char prefix = symbol->m_global_scope_p ? '&' : '%';

  if (symbol->m_decl && TREE_CODE (symbol->m_decl) == RESULT_DECL)
    {
      if (res_name_offset == 0)
	res_name_offset = brig_emit_string (symbol->m_name, '%');
      name_offset = res_name_offset;
    }
  else if (symbol->m_name)
    name_offset = brig_emit_string (symbol->m_name, prefix);
  else
    {
      char buf[64];
      snprintf (buf, 64, "__%s_%i", hsa_seg_name (symbol->m_segment),
		symbol->m_name_number);
      name_offset = brig_emit_string (buf, prefix);
    }

  dirvar.name = lendian32 (name_offset);
  dirvar.init = 0;
  dirvar.type = lendian16 (symbol->m_type);
  dirvar.segment = symbol->m_segment;
  /* TODO: Once we are able to access global variables, we must copy their
     alignment.  */
  dirvar.align = MAX (hsa_natural_alignment (dirvar.type),
		      (BrigAlignment8_t) BRIG_ALIGNMENT_4);
  dirvar.linkage = symbol->m_linkage;
  dirvar.dim.lo = symbol->m_dim;
  dirvar.dim.hi = symbol->m_dim >> 32;

  /* Global variables are just declared and linked via HSA runtime.  */
  if (symbol->m_linkage != BRIG_ALLOCATION_PROGRAM)
    dirvar.modifier |= BRIG_VARIABLE_DEFINITION;
  dirvar.reserved = 0;

  if (symbol->m_cst_value)
    {
      dirvar.modifier |= BRIG_VARIABLE_CONST;
      dirvar.init = lendian32 (enqueue_op (symbol->m_cst_value));
    }

  symbol->m_directive_offset = brig_code.add (&dirvar, sizeof (dirvar));
  return symbol->m_directive_offset;
}

/* Emit directives describing either a function declaration or
   definition F.  */

static BrigDirectiveExecutable *
emit_function_directives (hsa_function_representation *f, bool is_declaration)
{
  struct BrigDirectiveExecutable fndir;
  unsigned name_offset, inarg_off, scoped_off, next_toplev_off;
  int count = 0;
  BrigDirectiveExecutable *ptr_to_fndir;
  hsa_symbol *sym;

  if (!f->m_declaration_p)
    for (int i = 0; f->m_global_symbols.iterate (i, &sym); i++)
      {
	emit_directive_variable (sym);
	brig_insn_count++;
      }

  name_offset = brig_emit_string (f->m_name, '&');
  inarg_off = brig_code.total_size + sizeof (fndir)
    + (f->m_output_arg ? sizeof (struct BrigDirectiveVariable) : 0);
  scoped_off = inarg_off
    + f->m_input_args.length () * sizeof (struct BrigDirectiveVariable);

  if (!f->m_declaration_p)
    {
      count += f->m_spill_symbols.length ();
      count += f->m_private_variables.length ();
    }

  next_toplev_off = scoped_off + count * sizeof (struct BrigDirectiveVariable);

  memset (&fndir, 0, sizeof (fndir));
  fndir.base.byteCount = lendian16 (sizeof (fndir));
  fndir.base.kind = lendian16 (f->m_kern_p ? BRIG_KIND_DIRECTIVE_KERNEL
			       : BRIG_KIND_DIRECTIVE_FUNCTION);
  fndir.name = lendian32 (name_offset);
  fndir.inArgCount = lendian16 (f->m_input_args.length ());
  fndir.outArgCount = lendian16 (f->m_output_arg ? 1 : 0);
  fndir.firstInArg = lendian32 (inarg_off);
  fndir.firstCodeBlockEntry = lendian32 (scoped_off);
  fndir.nextModuleEntry = lendian32 (next_toplev_off);
  fndir.linkage = f->get_linkage ();
  if (!f->m_declaration_p)
    fndir.modifier |= BRIG_EXECUTABLE_DEFINITION;
  memset (&fndir.reserved, 0, sizeof (fndir.reserved));

  /* Once we put a definition of function_offsets, we should not overwrite
     it with a declaration of the function.  */
  if (f->m_internal_fn == NULL)
    {
      if (!function_offsets->get (f->m_decl) || !is_declaration)
	function_offsets->put (f->m_decl, brig_code.total_size);
    }
  else
    {
      /* Internal function.  */
      hsa_internal_fn **slot
	= hsa_emitted_internal_decls->find_slot (f->m_internal_fn, INSERT);
      hsa_internal_fn *int_fn = new hsa_internal_fn (f->m_internal_fn);
      int_fn->m_offset = brig_code.total_size;
      *slot = int_fn;
    }

  brig_code.add (&fndir, sizeof (fndir));
  /* terrible hack: we need to set instCount after we emit all
     insns, but we need to emit directive in order, and we emit directives
     during insn emitting.  So we need to emit the FUNCTION directive
     early, then the insns, and then we need to set instCount, so remember
     a pointer to it, in some horrible way.  cur_chunk.data+size points
     directly to after fndir here.  */
  ptr_to_fndir
      = (BrigDirectiveExecutable *)(brig_code.cur_chunk->data
				    + brig_code.cur_chunk->size
				    - sizeof (fndir));

  if (f->m_output_arg)
    emit_directive_variable (f->m_output_arg);
  for (unsigned i = 0; i < f->m_input_args.length (); i++)
    emit_directive_variable (f->m_input_args[i]);

  if (!f->m_declaration_p)
    {
      for (int i = 0; f->m_spill_symbols.iterate (i, &sym); i++)
	{
	  emit_directive_variable (sym);
	  brig_insn_count++;
	}
      for (unsigned i = 0; i < f->m_private_variables.length (); i++)
	{
	  emit_directive_variable (f->m_private_variables[i]);
	  brig_insn_count++;
	}
    }

  return ptr_to_fndir;
}

/* Emit a label directive for the given HBB.  We assume it is about to start on
   the current offset in the code section.  */

static void
emit_bb_label_directive (hsa_bb *hbb)
{
  struct BrigDirectiveLabel lbldir;

  lbldir.base.byteCount = lendian16 (sizeof (lbldir));
  lbldir.base.kind = lendian16 (BRIG_KIND_DIRECTIVE_LABEL);
  char buf[32];
  snprintf (buf, 32, "BB_%u_%i", DECL_UID (current_function_decl),
	    hbb->m_index);
  lbldir.name = lendian32 (brig_emit_string (buf, '@'));

  hbb->m_label_ref.m_directive_offset = brig_code.add (&lbldir,
						       sizeof (lbldir));
  brig_insn_count++;
}

/* Map a normal HSAIL type to the type of the equivalent BRIG operand
   holding such, for constants and registers.  */

static BrigType16_t
regtype_for_type (BrigType16_t t)
{
  switch (t)
    {
    case BRIG_TYPE_B1:
      return BRIG_TYPE_B1;

    case BRIG_TYPE_U8:
    case BRIG_TYPE_U16:
    case BRIG_TYPE_U32:
    case BRIG_TYPE_S8:
    case BRIG_TYPE_S16:
    case BRIG_TYPE_S32:
    case BRIG_TYPE_B8:
    case BRIG_TYPE_B16:
    case BRIG_TYPE_B32:
    case BRIG_TYPE_F16:
    case BRIG_TYPE_F32:
    case BRIG_TYPE_U8X4:
    case BRIG_TYPE_U16X2:
    case BRIG_TYPE_S8X4:
    case BRIG_TYPE_S16X2:
    case BRIG_TYPE_F16X2:
      return BRIG_TYPE_B32;

    case BRIG_TYPE_U64:
    case BRIG_TYPE_S64:
    case BRIG_TYPE_F64:
    case BRIG_TYPE_B64:
    case BRIG_TYPE_U8X8:
    case BRIG_TYPE_U16X4:
    case BRIG_TYPE_U32X2:
    case BRIG_TYPE_S8X8:
    case BRIG_TYPE_S16X4:
    case BRIG_TYPE_S32X2:
    case BRIG_TYPE_F16X4:
    case BRIG_TYPE_F32X2:
      return BRIG_TYPE_B64;

    case BRIG_TYPE_B128:
    case BRIG_TYPE_U8X16:
    case BRIG_TYPE_U16X8:
    case BRIG_TYPE_U32X4:
    case BRIG_TYPE_U64X2:
    case BRIG_TYPE_S8X16:
    case BRIG_TYPE_S16X8:
    case BRIG_TYPE_S32X4:
    case BRIG_TYPE_S64X2:
    case BRIG_TYPE_F16X8:
    case BRIG_TYPE_F32X4:
    case BRIG_TYPE_F64X2:
      return BRIG_TYPE_B128;

    default:
      gcc_unreachable ();
    }
}

/* Return the length of the BRIG type TYPE that is going to be streamed out as
   an immediate constant (so it must not be B1).  */

unsigned
hsa_get_imm_brig_type_len (BrigType16_t type)
{
  BrigType16_t base_type = type & BRIG_TYPE_BASE_MASK;
  BrigType16_t pack_type = type & BRIG_TYPE_PACK_MASK;

  switch (pack_type)
    {
    case BRIG_TYPE_PACK_NONE:
      break;
    case BRIG_TYPE_PACK_32:
      return 4;
    case BRIG_TYPE_PACK_64:
      return 8;
    case BRIG_TYPE_PACK_128:
      return 16;
    default:
      gcc_unreachable ();
    }

  switch (base_type)
    {
    case BRIG_TYPE_U8:
    case BRIG_TYPE_S8:
    case BRIG_TYPE_B8:
      return 1;
    case BRIG_TYPE_U16:
    case BRIG_TYPE_S16:
    case BRIG_TYPE_F16:
    case BRIG_TYPE_B16:
      return 2;
    case BRIG_TYPE_U32:
    case BRIG_TYPE_S32:
    case BRIG_TYPE_F32:
    case BRIG_TYPE_B32:
      return 4;
    case BRIG_TYPE_U64:
    case BRIG_TYPE_S64:
    case BRIG_TYPE_F64:
    case BRIG_TYPE_B64:
      return 8;
    case BRIG_TYPE_B128:
      return 16;
    default:
      gcc_unreachable ();
    }
}

/* Emit one scalar VALUE to the buffer DATA intended for BRIG emission.
   If NEED_LEN is not equal to zero, shrink or extend the value
   to NEED_LEN bytes.  Return how many bytes were written.  */

static int
emit_immediate_scalar_to_buffer (tree value, char *data, unsigned need_len)
{
  union hsa_bytes bytes;

  memset (&bytes, 0, sizeof (bytes));
  tree type = TREE_TYPE (value);
  gcc_checking_assert (TREE_CODE (type) != VECTOR_TYPE);

  unsigned data_len = tree_to_uhwi (TYPE_SIZE (type)) / BITS_PER_UNIT;
  if (INTEGRAL_TYPE_P (type)
      || (POINTER_TYPE_P (type) && TREE_CODE (value) == INTEGER_CST))
    switch (data_len)
      {
      case 1:
	bytes.b8 = (uint8_t) TREE_INT_CST_LOW (value);
	break;
      case 2:
	bytes.b16 = (uint16_t) TREE_INT_CST_LOW (value);
	break;
      case 4:
	bytes.b32 = (uint32_t) TREE_INT_CST_LOW (value);
	break;
      case 8:
	bytes.b64 = (uint64_t) TREE_INT_CST_LOW (value);
	break;
      default:
	gcc_unreachable ();
      }
  else if (SCALAR_FLOAT_TYPE_P (type))
    {
      if (data_len == 2)
	{
	  sorry ("Support for HSA does not implement immediate 16 bit FPU "
		 "operands");
	  return 2;
	}
      unsigned int_len = GET_MODE_SIZE (TYPE_MODE (type));
      /* There are always 32 bits in each long, no matter the size of
	 the hosts long.  */
      long tmp[6];

      real_to_target (tmp, TREE_REAL_CST_PTR (value), TYPE_MODE (type));

      if (int_len == 4)
	bytes.b32 = (uint32_t) tmp[0];
      else
	{
	  bytes.b64 = (uint64_t)(uint32_t) tmp[1];
	  bytes.b64 <<= 32;
	  bytes.b64 |= (uint32_t) tmp[0];
	}
    }
  else
    gcc_unreachable ();

  int len;
  if (need_len == 0)
    len = data_len;
  else
    len = need_len;

  memcpy (data, &bytes, len);
  return len;
}

void
hsa_op_immed::emit_to_buffer (tree value)
{
  unsigned total_len = m_brig_repr_size;

  /* As we can have a constructor with fewer elements, fill the memory
     with zeros.  */
  m_brig_repr = XCNEWVEC (char, total_len);
  char *p = m_brig_repr;

  if (TREE_CODE (value) == VECTOR_CST)
    {
      int i, num = VECTOR_CST_NELTS (value);
      for (i = 0; i < num; i++)
	{
	  unsigned actual;
	  actual
	    = emit_immediate_scalar_to_buffer (VECTOR_CST_ELT (value, i), p, 0);
	  total_len -= actual;
	  p += actual;
	}
      /* Vectors should have the exact size.  */
      gcc_assert (total_len == 0);
    }
  else if (TREE_CODE (value) == STRING_CST)
    memcpy (m_brig_repr, TREE_STRING_POINTER (value),
	    TREE_STRING_LENGTH (value));
  else if (TREE_CODE (value) == COMPLEX_CST)
    {
      gcc_assert (total_len % 2 == 0);
      unsigned actual;
      actual
	= emit_immediate_scalar_to_buffer (TREE_REALPART (value), p,
					   total_len / 2);

      gcc_assert (actual == total_len / 2);
      p += actual;

      actual
	= emit_immediate_scalar_to_buffer (TREE_IMAGPART (value), p,
					   total_len / 2);
      gcc_assert (actual == total_len / 2);
    }
  else if (TREE_CODE (value) == CONSTRUCTOR)
    {
      unsigned len = vec_safe_length (CONSTRUCTOR_ELTS (value));
      for (unsigned i = 0; i < len; i++)
	{
	  tree v = CONSTRUCTOR_ELT (value, i)->value;
	  unsigned actual = emit_immediate_scalar_to_buffer (v, p, 0);
	  total_len -= actual;
	  p += actual;
	}
    }
  else
    emit_immediate_scalar_to_buffer (value, p, total_len);
}

/* Emit an immediate BRIG operand IMM.  The BRIG type of the immediate might
   have been massaged to comply with various HSA/BRIG type requirements, so the
   only important aspect of that is the length (because HSAIL might expect
   smaller constants or become bit-data).  The data should be represented
   according to what is in the tree representation.  */

static void
emit_immediate_operand (hsa_op_immed *imm)
{
  struct BrigOperandConstantBytes out;

  memset (&out, 0, sizeof (out));
  out.base.byteCount = lendian16 (sizeof (out));
  out.base.kind = lendian16 (BRIG_KIND_OPERAND_CONSTANT_BYTES);
  uint32_t byteCount = lendian32 (imm->m_brig_repr_size);
  out.type = lendian16 (imm->m_type);
  out.bytes = lendian32 (brig_data.add (&byteCount, sizeof (byteCount)));
  brig_operand.add (&out, sizeof (out));
  brig_data.add (imm->m_brig_repr, imm->m_brig_repr_size);
  brig_data.round_size_up (4);
}

/* Emit a register BRIG operand REG.  */

static void
emit_register_operand (hsa_op_reg *reg)
{
  struct BrigOperandRegister out;

  out.base.byteCount = lendian16 (sizeof (out));
  out.base.kind = lendian16 (BRIG_KIND_OPERAND_REGISTER);
  out.regNum = lendian32 (reg->m_hard_num);

  switch (regtype_for_type (reg->m_type))
    {
    case BRIG_TYPE_B32:
      out.regKind = BRIG_REGISTER_KIND_SINGLE;
      break;
    case BRIG_TYPE_B64:
      out.regKind = BRIG_REGISTER_KIND_DOUBLE;
      break;
    case BRIG_TYPE_B128:
      out.regKind = BRIG_REGISTER_KIND_QUAD;
      break;
    case BRIG_TYPE_B1:
      out.regKind = BRIG_REGISTER_KIND_CONTROL;
      break;
    default:
      gcc_unreachable ();
    }

  brig_operand.add (&out, sizeof (out));
}

/* Emit an address BRIG operand ADDR.  */

static void
emit_address_operand (hsa_op_address *addr)
{
  struct BrigOperandAddress out;

  out.base.byteCount = lendian16 (sizeof (out));
  out.base.kind = lendian16 (BRIG_KIND_OPERAND_ADDRESS);
  out.symbol = addr->m_symbol
    ? lendian32 (emit_directive_variable (addr->m_symbol)) : 0;
  out.reg = addr->m_reg ? lendian32 (enqueue_op (addr->m_reg)) : 0;

  if (sizeof (addr->m_imm_offset) == 8)
    {
      out.offset.lo = lendian32 (addr->m_imm_offset);
      out.offset.hi = lendian32 (addr->m_imm_offset >> 32);
    }
  else
    {
      gcc_assert (sizeof (addr->m_imm_offset) == 4);
      out.offset.lo = lendian32 (addr->m_imm_offset);
      out.offset.hi = 0;
    }

  brig_operand.add (&out, sizeof (out));
}

/* Emit a code reference operand REF.  */

static void
emit_code_ref_operand (hsa_op_code_ref *ref)
{
  struct BrigOperandCodeRef out;

  out.base.byteCount = lendian16 (sizeof (out));
  out.base.kind = lendian16 (BRIG_KIND_OPERAND_CODE_REF);
  out.ref = lendian32 (ref->m_directive_offset);
  brig_operand.add (&out, sizeof (out));
}

/* Emit a code list operand CODE_LIST.  */

static void
emit_code_list_operand (hsa_op_code_list *code_list)
{
  struct BrigOperandCodeList out;
  unsigned args = code_list->m_offsets.length ();

  for (unsigned i = 0; i < args; i++)
    gcc_assert (code_list->m_offsets[i]);

  out.base.byteCount = lendian16 (sizeof (out));
  out.base.kind = lendian16 (BRIG_KIND_OPERAND_CODE_LIST);

  uint32_t byteCount = lendian32 (4 * args);

  out.elements = lendian32 (brig_data.add (&byteCount, sizeof (byteCount)));
  brig_data.add (code_list->m_offsets.address (), args * sizeof (uint32_t));
  brig_data.round_size_up (4);
  brig_operand.add (&out, sizeof (out));
}

/* Emit an operand list operand OPERAND_LIST.  */

static void
emit_operand_list_operand (hsa_op_operand_list *operand_list)
{
  struct BrigOperandOperandList out;
  unsigned args = operand_list->m_offsets.length ();

  for (unsigned i = 0; i < args; i++)
    gcc_assert (operand_list->m_offsets[i]);

  out.base.byteCount = lendian16 (sizeof (out));
  out.base.kind = lendian16 (BRIG_KIND_OPERAND_OPERAND_LIST);

  uint32_t byteCount = lendian32 (4 * args);

  out.elements = lendian32 (brig_data.add (&byteCount, sizeof (byteCount)));
  brig_data.add (operand_list->m_offsets.address (), args * sizeof (uint32_t));
  brig_data.round_size_up (4);
  brig_operand.add (&out, sizeof (out));
}

/* Emit all operands queued for writing.  */

static void
emit_queued_operands (void)
{
  for (hsa_op_base *op = op_queue.first_op; op; op = op->m_next)
    {
      gcc_assert (op->m_brig_op_offset == brig_operand.total_size);
      if (hsa_op_immed *imm = dyn_cast <hsa_op_immed *> (op))
	emit_immediate_operand (imm);
      else if (hsa_op_reg *reg = dyn_cast <hsa_op_reg *> (op))
	emit_register_operand (reg);
      else if (hsa_op_address *addr = dyn_cast <hsa_op_address *> (op))
	emit_address_operand (addr);
      else if (hsa_op_code_ref *ref = dyn_cast <hsa_op_code_ref *> (op))
	emit_code_ref_operand (ref);
      else if (hsa_op_code_list *code_list = dyn_cast <hsa_op_code_list *> (op))
	emit_code_list_operand (code_list);
      else if (hsa_op_operand_list *l = dyn_cast <hsa_op_operand_list *> (op))
	emit_operand_list_operand (l);
      else
	gcc_unreachable ();
    }
}

/* Emit directives describing the function that is used for
   a function declaration.  */

static BrigDirectiveExecutable *
emit_function_declaration (tree decl)
{
  hsa_function_representation *f = hsa_generate_function_declaration (decl);

  BrigDirectiveExecutable *e = emit_function_directives (f, true);
  emit_queued_operands ();

  delete f;

  return e;
}

/* Emit directives describing the function that is used for
   an internal function declaration.  */

static BrigDirectiveExecutable *
emit_internal_fn_decl (hsa_internal_fn *fn)
{
  hsa_function_representation *f = hsa_generate_internal_fn_decl (fn);

  BrigDirectiveExecutable *e = emit_function_directives (f, true);
  emit_queued_operands ();

  delete f;

  return e;
}

/* Enqueue all operands of INSN and return offset to BRIG data section
   to list of operand offsets.  */

static unsigned
emit_insn_operands (hsa_insn_basic *insn)
{
  auto_vec<BrigOperandOffset32_t, HSA_BRIG_INT_STORAGE_OPERANDS>
    operand_offsets;

  unsigned l = insn->operand_count ();
  operand_offsets.safe_grow (l);

  for (unsigned i = 0; i < l; i++)
    operand_offsets[i] = lendian32 (enqueue_op (insn->get_op (i)));

  /* We have N operands so use 4 * N for the byte_count.  */
  uint32_t byte_count = lendian32 (4 * l);

  unsigned offset = brig_data.add (&byte_count, sizeof (byte_count));
  brig_data.add (operand_offsets.address (),
		 l * sizeof (BrigOperandOffset32_t));

  brig_data.round_size_up (4);

  return offset;
}

/* Enqueue operand OP0, OP1, OP2 (if different from NULL) and return offset
   to BRIG data section to list of operand offsets.  */

static unsigned
emit_operands (hsa_op_base *op0, hsa_op_base *op1 = NULL,
	       hsa_op_base *op2 = NULL)
{
  auto_vec<BrigOperandOffset32_t, HSA_BRIG_INT_STORAGE_OPERANDS>
    operand_offsets;

  gcc_checking_assert (op0 != NULL);
  operand_offsets.safe_push (enqueue_op (op0));

  if (op1 != NULL)
    {
      operand_offsets.safe_push (enqueue_op (op1));
      if (op2 != NULL)
	operand_offsets.safe_push (enqueue_op (op2));
    }

  unsigned l = operand_offsets.length ();

  /* We have N operands so use 4 * N for the byte_count.  */
  uint32_t byte_count = lendian32 (4 * l);

  unsigned offset = brig_data.add (&byte_count, sizeof (byte_count));
  brig_data.add (operand_offsets.address (),
		 l * sizeof (BrigOperandOffset32_t));

  brig_data.round_size_up (4);

  return offset;
}

/* Emit an HSA memory instruction and all necessary directives, schedule
   necessary operands for writing.  */

static void
emit_memory_insn (hsa_insn_mem *mem)
{
  struct BrigInstMem repr;
  gcc_checking_assert (mem->operand_count () == 2);

  hsa_op_address *addr = as_a <hsa_op_address *> (mem->get_op (1));

  /* This is necessary because of the erroneous typedef of
     BrigMemoryModifier8_t which introduces padding which may then contain
     random stuff (which we do not want so that we can test things don't
     change).  */
  memset (&repr, 0, sizeof (repr));
  repr.base.base.byteCount = lendian16 (sizeof (repr));
  repr.base.base.kind = lendian16 (BRIG_KIND_INST_MEM);
  repr.base.opcode = lendian16 (mem->m_opcode);
  repr.base.type = lendian16 (mem->m_type);
  repr.base.operands = lendian32 (emit_insn_operands (mem));

  if (addr->m_symbol)
    repr.segment = addr->m_symbol->m_segment;
  else
    repr.segment = BRIG_SEGMENT_FLAT;
  repr.modifier = 0;
  repr.equivClass = mem->m_equiv_class;
  repr.align = mem->m_align;
  if (mem->m_opcode == BRIG_OPCODE_LD)
    repr.width = BRIG_WIDTH_1;
  else
    repr.width = BRIG_WIDTH_NONE;
  memset (&repr.reserved, 0, sizeof (repr.reserved));
  brig_code.add (&repr, sizeof (repr));
  brig_insn_count++;
}

/* Emit an HSA signal memory instruction and all necessary directives, schedule
   necessary operands for writing.  */

static void
emit_signal_insn (hsa_insn_signal *mem)
{
  struct BrigInstSignal repr;

  /* This is necessary because of the erroneous typedef of
     BrigMemoryModifier8_t which introduces padding which may then contain
     random stuff (which we do not want so that we can test things don't
     change).  */
  memset (&repr, 0, sizeof (repr));
  repr.base.base.byteCount = lendian16 (sizeof (repr));
  repr.base.base.kind = lendian16 (BRIG_KIND_INST_SIGNAL);
  repr.base.opcode = lendian16 (mem->m_opcode);
  repr.base.type = lendian16 (mem->m_type);
  repr.base.operands = lendian32 (emit_insn_operands (mem));

  repr.memoryOrder = mem->m_memoryorder;
  repr.signalOperation = mem->m_atomicop;
  repr.signalType = BRIG_TYPE_SIG64;

  brig_code.add (&repr, sizeof (repr));
  brig_insn_count++;
}

/* Emit an HSA atomic memory instruction and all necessary directives, schedule
   necessary operands for writing.  */

static void
emit_atomic_insn (hsa_insn_atomic *mem)
{
  struct BrigInstAtomic repr;

  /* Either operand[0] or operand[1] must be an address operand.  */
  hsa_op_address *addr = NULL;
  if (is_a <hsa_op_address *> (mem->get_op (0)))
    addr = as_a <hsa_op_address *> (mem->get_op (0));
  else
    addr = as_a <hsa_op_address *> (mem->get_op (1));

  /* This is necessary because of the erroneous typedef of
     BrigMemoryModifier8_t which introduces padding which may then contain
     random stuff (which we do not want so that we can test things don't
     change).  */
  memset (&repr, 0, sizeof (repr));
  repr.base.base.byteCount = lendian16 (sizeof (repr));
  repr.base.base.kind = lendian16 (BRIG_KIND_INST_ATOMIC);
  repr.base.opcode = lendian16 (mem->m_opcode);
  repr.base.type = lendian16 (mem->m_type);
  repr.base.operands = lendian32 (emit_insn_operands (mem));

  if (addr->m_symbol)
    repr.segment = addr->m_symbol->m_segment;
  else
    repr.segment = BRIG_SEGMENT_FLAT;
  repr.memoryOrder = mem->m_memoryorder;
  repr.memoryScope = mem->m_memoryscope;
  repr.atomicOperation = mem->m_atomicop;

  brig_code.add (&repr, sizeof (repr));
  brig_insn_count++;
}

/* Emit an HSA LDA instruction and all necessary directives, schedule
   necessary operands for writing.  */

static void
emit_addr_insn (hsa_insn_basic *insn)
{
  struct BrigInstAddr repr;

  hsa_op_address *addr = as_a <hsa_op_address *> (insn->get_op (1));

  repr.base.base.byteCount = lendian16 (sizeof (repr));
  repr.base.base.kind = lendian16 (BRIG_KIND_INST_ADDR);
  repr.base.opcode = lendian16 (insn->m_opcode);
  repr.base.type = lendian16 (insn->m_type);
  repr.base.operands = lendian32 (emit_insn_operands (insn));

  if (addr->m_symbol)
    repr.segment = addr->m_symbol->m_segment;
  else
    repr.segment = BRIG_SEGMENT_FLAT;
  memset (&repr.reserved, 0, sizeof (repr.reserved));

  brig_code.add (&repr, sizeof (repr));
  brig_insn_count++;
}

/* Emit an HSA segment conversion instruction and all necessary directives,
   schedule necessary operands for writing.  */

static void
emit_segment_insn (hsa_insn_seg *seg)
{
  struct BrigInstSegCvt repr;

  repr.base.base.byteCount = lendian16 (sizeof (repr));
  repr.base.base.kind = lendian16 (BRIG_KIND_INST_SEG_CVT);
  repr.base.opcode = lendian16 (seg->m_opcode);
  repr.base.type = lendian16 (seg->m_type);
  repr.base.operands = lendian32 (emit_insn_operands (seg));
  repr.sourceType = lendian16 (as_a <hsa_op_reg *> (seg->get_op (1))->m_type);
  repr.segment = seg->m_segment;
  repr.modifier = 0;

  brig_code.add (&repr, sizeof (repr));

  brig_insn_count++;
}

/* Emit an HSA alloca instruction and all necessary directives,
   schedule necessary operands for writing.  */

static void
emit_alloca_insn (hsa_insn_alloca *alloca)
{
  struct BrigInstMem repr;
  gcc_checking_assert (alloca->operand_count () == 2);

  /* This is necessary because of the erroneous typedef of
     BrigMemoryModifier8_t which introduces padding which may then contain
     random stuff (which we do not want so that we can test things don't
     change).  */
  memset (&repr, 0, sizeof (repr));
  repr.base.base.byteCount = lendian16 (sizeof (repr));
  repr.base.base.kind = lendian16 (BRIG_KIND_INST_MEM);
  repr.base.opcode = lendian16 (alloca->m_opcode);
  repr.base.type = lendian16 (alloca->m_type);
  repr.base.operands = lendian32 (emit_insn_operands (alloca));
  repr.segment = BRIG_SEGMENT_PRIVATE;
  repr.modifier = 0;
  repr.equivClass = 0;
  repr.align = alloca->m_align;
  repr.width = BRIG_WIDTH_NONE;
  memset (&repr.reserved, 0, sizeof (repr.reserved));
  brig_code.add (&repr, sizeof (repr));
  brig_insn_count++;
}

/* Emit an HSA comparison instruction and all necessary directives,
   schedule necessary operands for writing.  */

static void
emit_cmp_insn (hsa_insn_cmp *cmp)
{
  struct BrigInstCmp repr;

  memset (&repr, 0, sizeof (repr));
  repr.base.base.byteCount = lendian16 (sizeof (repr));
  repr.base.base.kind = lendian16 (BRIG_KIND_INST_CMP);
  repr.base.opcode = lendian16 (cmp->m_opcode);
  repr.base.type = lendian16 (cmp->m_type);
  repr.base.operands = lendian32 (emit_insn_operands (cmp));

  if (is_a <hsa_op_reg *> (cmp->get_op (1)))
    repr.sourceType
      = lendian16 (as_a <hsa_op_reg *> (cmp->get_op (1))->m_type);
  else
    repr.sourceType
      = lendian16 (as_a <hsa_op_immed *> (cmp->get_op (1))->m_type);
  repr.modifier = 0;
  repr.compare = cmp->m_compare;
  repr.pack = 0;

  brig_code.add (&repr, sizeof (repr));
  brig_insn_count++;
}

/* Emit an HSA branching instruction and all necessary directives, schedule
   necessary operands for writing.  */

static void
emit_branch_insn (hsa_insn_br *br)
{
  struct BrigInstBr repr;

  basic_block target = NULL;
  edge_iterator ei;
  edge e;

  /* At the moment we only handle direct conditional jumps.  */
  gcc_assert (br->m_opcode == BRIG_OPCODE_CBR);
  repr.base.base.byteCount = lendian16 (sizeof (repr));
  repr.base.base.kind = lendian16 (BRIG_KIND_INST_BR);
  repr.base.opcode = lendian16 (br->m_opcode);
  repr.width = BRIG_WIDTH_1;
  /* For Conditional jumps the type is always B1.  */
  repr.base.type = lendian16 (BRIG_TYPE_B1);

  FOR_EACH_EDGE (e, ei, br->m_bb->succs)
    if (e->flags & EDGE_TRUE_VALUE)
      {
	target = e->dest;
	break;
      }
  gcc_assert (target);

  repr.base.operands
    = lendian32 (emit_operands (br->get_op (0),
				&hsa_bb_for_bb (target)->m_label_ref));
  memset (&repr.reserved, 0, sizeof (repr.reserved));

  brig_code.add (&repr, sizeof (repr));
  brig_insn_count++;
}

/* Emit an HSA unconditional jump branching instruction that points to
   a label REFERENCE.  */

static void
emit_unconditional_jump (hsa_op_code_ref *reference)
{
  struct BrigInstBr repr;

  repr.base.base.byteCount = lendian16 (sizeof (repr));
  repr.base.base.kind = lendian16 (BRIG_KIND_INST_BR);
  repr.base.opcode = lendian16 (BRIG_OPCODE_BR);
  repr.base.type = lendian16 (BRIG_TYPE_NONE);
  /* Direct branches to labels must be width(all).  */
  repr.width = BRIG_WIDTH_ALL;

  repr.base.operands = lendian32 (emit_operands (reference));
  memset (&repr.reserved, 0, sizeof (repr.reserved));
  brig_code.add (&repr, sizeof (repr));
  brig_insn_count++;
}

/* Emit an HSA switch jump instruction that uses a jump table to
   jump to a destination label.  */

static void
emit_switch_insn (hsa_insn_sbr *sbr)
{
  struct BrigInstBr repr;

  gcc_assert (sbr->m_opcode == BRIG_OPCODE_SBR);
  repr.base.base.byteCount = lendian16 (sizeof (repr));
  repr.base.base.kind = lendian16 (BRIG_KIND_INST_BR);
  repr.base.opcode = lendian16 (sbr->m_opcode);
  repr.width = BRIG_WIDTH_1;
  /* For Conditional jumps the type is always B1.  */
  hsa_op_reg *index = as_a <hsa_op_reg *> (sbr->get_op (0));
  repr.base.type = lendian16 (index->m_type);
  repr.base.operands
    = lendian32 (emit_operands (sbr->get_op (0), sbr->m_label_code_list));
  memset (&repr.reserved, 0, sizeof (repr.reserved));

  brig_code.add (&repr, sizeof (repr));
  brig_insn_count++;

  /* Emit jump to default label.  */
  hsa_bb *hbb = hsa_bb_for_bb (sbr->m_default_bb);
  emit_unconditional_jump (&hbb->m_label_ref);
}

/* Emit a HSA convert instruction and all necessary directives, schedule
   necessary operands for writing.  */

static void
emit_cvt_insn (hsa_insn_cvt *insn)
{
  struct BrigInstCvt repr;
  BrigType16_t srctype;

  repr.base.base.byteCount = lendian16 (sizeof (repr));
  repr.base.base.kind = lendian16 (BRIG_KIND_INST_CVT);
  repr.base.opcode = lendian16 (insn->m_opcode);
  repr.base.type = lendian16 (insn->m_type);
  repr.base.operands = lendian32 (emit_insn_operands (insn));

  if (is_a <hsa_op_reg *> (insn->get_op (1)))
    srctype = as_a <hsa_op_reg *> (insn->get_op (1))->m_type;
  else
    srctype = as_a <hsa_op_immed *> (insn->get_op (1))->m_type;
  repr.sourceType = lendian16 (srctype);
  repr.modifier = 0;
  /* float to smaller float requires a rounding setting (we default
     to 'near'.  */
  if (hsa_type_float_p (insn->m_type)
      && (!hsa_type_float_p (srctype)
	  || ((insn->m_type & BRIG_TYPE_BASE_MASK)
	      < (srctype & BRIG_TYPE_BASE_MASK))))
    repr.round = BRIG_ROUND_FLOAT_NEAR_EVEN;
  else if (hsa_type_integer_p (insn->m_type) &&
	   hsa_type_float_p (srctype))
    repr.round = BRIG_ROUND_INTEGER_ZERO;
  else
    repr.round = BRIG_ROUND_NONE;
  brig_code.add (&repr, sizeof (repr));
  brig_insn_count++;
}

/* Emit call instruction INSN, where this instruction must be closed
   within a call block instruction.  */

static void
emit_call_insn (hsa_insn_call *call)
{
  struct BrigInstBr repr;

  repr.base.base.byteCount = lendian16 (sizeof (repr));
  repr.base.base.kind = lendian16 (BRIG_KIND_INST_BR);
  repr.base.opcode = lendian16 (BRIG_OPCODE_CALL);
  repr.base.type = lendian16 (BRIG_TYPE_NONE);

  repr.base.operands
    = lendian32 (emit_operands (call->m_result_code_list, &call->m_func,
				call->m_args_code_list));

  /* Internal functions have not set m_called_function.  */
  if (call->m_called_function)
    {
      function_linkage_pair pair (call->m_called_function,
				  call->m_func.m_brig_op_offset);
      function_call_linkage.safe_push (pair);
    }
  else
    {
      hsa_internal_fn *slot
	= hsa_emitted_internal_decls->find (call->m_called_internal_fn);
      gcc_assert (slot);
      gcc_assert (slot->m_offset > 0);
      call->m_func.m_directive_offset = slot->m_offset;
    }

  repr.width = BRIG_WIDTH_ALL;
  memset (&repr.reserved, 0, sizeof (repr.reserved));

  brig_code.add (&repr, sizeof (repr));
  brig_insn_count++;
}

/* Emit argument block directive.  */

static void
emit_arg_block_insn (hsa_insn_arg_block *insn)
{
  switch (insn->m_kind)
    {
    case BRIG_KIND_DIRECTIVE_ARG_BLOCK_START:
      {
	struct BrigDirectiveArgBlock repr;
	repr.base.byteCount = lendian16 (sizeof (repr));
	repr.base.kind = lendian16 (insn->m_kind);
	brig_code.add (&repr, sizeof (repr));

	for (unsigned i = 0; i < insn->m_call_insn->m_input_args.length (); i++)
	  {
	    insn->m_call_insn->m_args_code_list->m_offsets[i]
	      = lendian32 (emit_directive_variable
			   (insn->m_call_insn->m_input_args[i]));
	    brig_insn_count++;
	  }

	if (insn->m_call_insn->m_output_arg)
	  {
	    insn->m_call_insn->m_result_code_list->m_offsets[0]
	      = lendian32 (emit_directive_variable
			   (insn->m_call_insn->m_output_arg));
	    brig_insn_count++;
	  }

	break;
      }
    case BRIG_KIND_DIRECTIVE_ARG_BLOCK_END:
      {
	struct BrigDirectiveArgBlock repr;
	repr.base.byteCount = lendian16 (sizeof (repr));
	repr.base.kind = lendian16 (insn->m_kind);
	brig_code.add (&repr, sizeof (repr));
	break;
      }
    default:
      gcc_unreachable ();
    }

  brig_insn_count++;
}

/* Emit comment directive.  */

static void
emit_comment_insn (hsa_insn_comment *insn)
{
  struct BrigDirectiveComment repr;
  memset (&repr, 0, sizeof (repr));

  repr.base.byteCount = lendian16 (sizeof (repr));
  repr.base.kind = lendian16 (insn->m_opcode);
  repr.name = brig_emit_string (insn->m_comment, '\0', false);
  brig_code.add (&repr, sizeof (repr));
}

/* Emit queue instruction INSN.  */

static void
emit_queue_insn (hsa_insn_queue *insn)
{
  BrigInstQueue repr;
  memset (&repr, 0, sizeof (repr));

  repr.base.base.byteCount = lendian16 (sizeof (repr));
  repr.base.base.kind = lendian16 (BRIG_KIND_INST_QUEUE);
  repr.base.opcode = lendian16 (insn->m_opcode);
  repr.base.type = lendian16 (insn->m_type);
  repr.segment = BRIG_SEGMENT_GLOBAL;
  repr.memoryOrder = BRIG_MEMORY_ORDER_SC_RELEASE;
  repr.base.operands = lendian32 (emit_insn_operands (insn));
  brig_data.round_size_up (4);
  brig_code.add (&repr, sizeof (repr));

  brig_insn_count++;
}

/* Emit source type instruction INSN.  */

static void
emit_srctype_insn (hsa_insn_srctype *insn)
{
  /* We assume that BrigInstMod has a BrigInstBasic prefix.  */
  struct BrigInstSourceType repr;
  unsigned operand_count = insn->operand_count ();
  gcc_checking_assert (operand_count >= 2);

  memset (&repr, 0, sizeof (repr));
  repr.sourceType = lendian16 (insn->m_source_type);
  repr.base.base.byteCount = lendian16 (sizeof (repr));
  repr.base.base.kind = lendian16 (BRIG_KIND_INST_SOURCE_TYPE);
  repr.base.opcode = lendian16 (insn->m_opcode);
  repr.base.type = lendian16 (insn->m_type);

  repr.base.operands = lendian32 (emit_insn_operands (insn));
  brig_code.add (&repr, sizeof (struct BrigInstSourceType));
  brig_insn_count++;
}

/* Emit packed instruction INSN.  */

static void
emit_packed_insn (hsa_insn_packed *insn)
{
  /* We assume that BrigInstMod has a BrigInstBasic prefix.  */
  struct BrigInstSourceType repr;
  unsigned operand_count = insn->operand_count ();
  gcc_checking_assert (operand_count >= 2);

  memset (&repr, 0, sizeof (repr));
  repr.sourceType = lendian16 (insn->m_source_type);
  repr.base.base.byteCount = lendian16 (sizeof (repr));
  repr.base.base.kind = lendian16 (BRIG_KIND_INST_SOURCE_TYPE);
  repr.base.opcode = lendian16 (insn->m_opcode);
  repr.base.type = lendian16 (insn->m_type);

  if (insn->m_opcode == BRIG_OPCODE_COMBINE)
    {
      /* Create operand list for packed type.  */
      for (unsigned i = 1; i < operand_count; i++)
	{
	  gcc_checking_assert (insn->get_op (i));
	  insn->m_operand_list->m_offsets[i - 1]
	    = lendian32 (enqueue_op (insn->get_op (i)));
	}

      repr.base.operands = lendian32 (emit_operands (insn->get_op (0),
						     insn->m_operand_list));
    }
  else if (insn->m_opcode == BRIG_OPCODE_EXPAND)
    {
      /* Create operand list for packed type.  */
      for (unsigned i = 0; i < operand_count - 1; i++)
	{
	  gcc_checking_assert (insn->get_op (i));
	  insn->m_operand_list->m_offsets[i]
	    = lendian32 (enqueue_op (insn->get_op (i)));
	}

      unsigned ops = emit_operands (insn->m_operand_list,
				    insn->get_op (insn->operand_count () - 1));
      repr.base.operands = lendian32 (ops);
    }


  brig_code.add (&repr, sizeof (struct BrigInstSourceType));
  brig_insn_count++;
}

/* Emit a basic HSA instruction and all necessary directives, schedule
   necessary operands for writing.  */

static void
emit_basic_insn (hsa_insn_basic *insn)
{
  /* We assume that BrigInstMod has a BrigInstBasic prefix.  */
  struct BrigInstMod repr;
  BrigType16_t type;

  memset (&repr, 0, sizeof (repr));
  repr.base.base.byteCount = lendian16 (sizeof (BrigInstBasic));
  repr.base.base.kind = lendian16 (BRIG_KIND_INST_BASIC);
  repr.base.opcode = lendian16 (insn->m_opcode);
  switch (insn->m_opcode)
    {
      /* And the bit-logical operations need bit types and whine about
	 arithmetic types :-/  */
      case BRIG_OPCODE_AND:
      case BRIG_OPCODE_OR:
      case BRIG_OPCODE_XOR:
      case BRIG_OPCODE_NOT:
	type = regtype_for_type (insn->m_type);
	break;
      default:
	type = insn->m_type;
	break;
    }
  repr.base.type = lendian16 (type);
  repr.base.operands = lendian32 (emit_insn_operands (insn));

  if (hsa_type_packed_p (type))
    {
      if (hsa_type_float_p (type)
	  && !hsa_opcode_floating_bit_insn_p (insn->m_opcode))
	repr.round = BRIG_ROUND_FLOAT_NEAR_EVEN;
      else
	repr.round = 0;
      /* We assume that destination and sources agree in packing layout.  */
      if (insn->num_used_ops () >= 2)
	repr.pack = BRIG_PACK_PP;
      else
	repr.pack = BRIG_PACK_P;
      repr.reserved = 0;
      repr.base.base.byteCount = lendian16 (sizeof (BrigInstMod));
      repr.base.base.kind = lendian16 (BRIG_KIND_INST_MOD);
      brig_code.add (&repr, sizeof (struct BrigInstMod));
    }
  else
    brig_code.add (&repr, sizeof (struct BrigInstBasic));
  brig_insn_count++;
}

/* Emit an HSA instruction and all necessary directives, schedule necessary
   operands for writing.  */

static void
emit_insn (hsa_insn_basic *insn)
{
  gcc_assert (!is_a <hsa_insn_phi *> (insn));

  insn->m_brig_offset = brig_code.total_size;

  if (hsa_insn_signal *signal = dyn_cast <hsa_insn_signal *> (insn))
    emit_signal_insn (signal);
  else if (hsa_insn_atomic *atom = dyn_cast <hsa_insn_atomic *> (insn))
    emit_atomic_insn (atom);
  else if (hsa_insn_mem *mem = dyn_cast <hsa_insn_mem *> (insn))
    emit_memory_insn (mem);
  else if (insn->m_opcode == BRIG_OPCODE_LDA)
    emit_addr_insn (insn);
  else if (hsa_insn_seg *seg = dyn_cast <hsa_insn_seg *> (insn))
    emit_segment_insn (seg);
  else if (hsa_insn_cmp *cmp = dyn_cast <hsa_insn_cmp *> (insn))
    emit_cmp_insn (cmp);
  else if (hsa_insn_br *br = dyn_cast <hsa_insn_br *> (insn))
    emit_branch_insn (br);
  else if (hsa_insn_sbr *sbr = dyn_cast <hsa_insn_sbr *> (insn))
    {
      if (switch_instructions == NULL)
	switch_instructions = new vec <hsa_insn_sbr *> ();

      switch_instructions->safe_push (sbr);
      emit_switch_insn (sbr);
    }
  else if (hsa_insn_arg_block *block = dyn_cast <hsa_insn_arg_block *> (insn))
    emit_arg_block_insn (block);
  else if (hsa_insn_call *call = dyn_cast <hsa_insn_call *> (insn))
    emit_call_insn (call);
  else if (hsa_insn_comment *comment = dyn_cast <hsa_insn_comment *> (insn))
    emit_comment_insn (comment);
  else if (hsa_insn_queue *queue = dyn_cast <hsa_insn_queue *> (insn))
    emit_queue_insn (queue);
  else if (hsa_insn_srctype *srctype = dyn_cast <hsa_insn_srctype *> (insn))
    emit_srctype_insn (srctype);
  else if (hsa_insn_packed *packed = dyn_cast <hsa_insn_packed *> (insn))
    emit_packed_insn (packed);
  else if (hsa_insn_cvt *cvt = dyn_cast <hsa_insn_cvt *> (insn))
    emit_cvt_insn (cvt);
  else if (hsa_insn_alloca *alloca = dyn_cast <hsa_insn_alloca *> (insn))
    emit_alloca_insn (alloca);
  else
    emit_basic_insn (insn);
}

/* We have just finished emitting BB and are about to emit NEXT_BB if non-NULL,
   or we are about to finish emitting code, if it is NULL.  If the fall through
   edge from BB does not lead to NEXT_BB, emit an unconditional jump.  */

static void
perhaps_emit_branch (basic_block bb, basic_block next_bb)
{
  basic_block t_bb = NULL, ff = NULL;

  edge_iterator ei;
  edge e;

  /* If the last instruction of BB is a switch, ignore emission of all
     edges.  */
  if (hsa_bb_for_bb (bb)->m_last_insn
      && is_a <hsa_insn_sbr *> (hsa_bb_for_bb (bb)->m_last_insn))
    return;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (e->flags & EDGE_TRUE_VALUE)
      {
	gcc_assert (!t_bb);
	t_bb = e->dest;
      }
    else
      {
	gcc_assert (!ff);
	ff = e->dest;
      }

  if (!ff || ff == next_bb || ff == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return;

  emit_unconditional_jump (&hsa_bb_for_bb (ff)->m_label_ref);
}

/* Emit the a function with name NAME to the various brig sections.  */

void
hsa_brig_emit_function (void)
{
  basic_block bb, prev_bb;
  hsa_insn_basic *insn;
  BrigDirectiveExecutable *ptr_to_fndir;

  brig_init ();

  brig_insn_count = 0;
  memset (&op_queue, 0, sizeof (op_queue));
  op_queue.projected_size = brig_operand.total_size;

  if (!function_offsets)
    function_offsets = new hash_map<tree, BrigCodeOffset32_t> ();

  if (!emitted_declarations)
    emitted_declarations = new hash_map <tree, BrigDirectiveExecutable *> ();

  for (unsigned i = 0; i < hsa_cfun->m_called_functions.length (); i++)
    {
      tree called = hsa_cfun->m_called_functions[i];

      /* If the function has no definition, emit a declaration.  */
      if (!emitted_declarations->get (called))
	{
	  BrigDirectiveExecutable *e = emit_function_declaration (called);
	  emitted_declarations->put (called, e);
	}
    }

  for (unsigned i = 0; i < hsa_cfun->m_called_internal_fns.length (); i++)
    {
      hsa_internal_fn *called = hsa_cfun->m_called_internal_fns[i];
      emit_internal_fn_decl (called);
    }

  ptr_to_fndir = emit_function_directives (hsa_cfun, false);
  for (insn = hsa_bb_for_bb (ENTRY_BLOCK_PTR_FOR_FN (cfun))->m_first_insn;
       insn;
       insn = insn->m_next)
    emit_insn (insn);
  prev_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  FOR_EACH_BB_FN (bb, cfun)
    {
      perhaps_emit_branch (prev_bb, bb);
      emit_bb_label_directive (hsa_bb_for_bb (bb));
      for (insn = hsa_bb_for_bb (bb)->m_first_insn; insn; insn = insn->m_next)
	emit_insn (insn);
      prev_bb = bb;
    }
  perhaps_emit_branch (prev_bb, NULL);
  ptr_to_fndir->nextModuleEntry = brig_code.total_size;

  /* Fill up label references for all sbr instructions.  */
  if (switch_instructions)
    {
      for (unsigned i = 0; i < switch_instructions->length (); i++)
	{
	  hsa_insn_sbr *sbr = (*switch_instructions)[i];
	  for (unsigned j = 0; j < sbr->m_jump_table.length (); j++)
	    {
	      hsa_bb *hbb = hsa_bb_for_bb (sbr->m_jump_table[j]);
	      sbr->m_label_code_list->m_offsets[j]
		= hbb->m_label_ref.m_directive_offset;
	    }
	}

      switch_instructions->release ();
      delete switch_instructions;
      switch_instructions = NULL;
    }

  if (dump_file)
    {
      fprintf (dump_file, "------- After BRIG emission: -------\n");
      dump_hsa_cfun (dump_file);
    }

  emit_queued_operands ();
}

/* Emit all OMP symbols related to OMP.  */

void
hsa_brig_emit_omp_symbols (void)
{
  brig_init ();
  emit_directive_variable (hsa_num_threads);
}

static GTY(()) tree hsa_cdtor_statements[2];

/* Create and return __hsa_global_variables symbol that contains
   all informations consumed by libgomp to link global variables
   with their string names used by an HSA kernel.  */

static tree
hsa_output_global_variables ()
{
  unsigned l = hsa_global_variable_symbols->elements ();

  tree variable_info_type = make_node (RECORD_TYPE);
  tree id_f1 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			   get_identifier ("name"), ptr_type_node);
  DECL_CHAIN (id_f1) = NULL_TREE;
  tree id_f2 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			   get_identifier ("omp_data_size"),
			   ptr_type_node);
  DECL_CHAIN (id_f2) = id_f1;
  finish_builtin_struct (variable_info_type, "__hsa_variable_info", id_f2,
			 NULL_TREE);

  tree int_num_of_global_vars;
  int_num_of_global_vars = build_int_cst (uint32_type_node, l);
  tree global_vars_num_index_type = build_index_type (int_num_of_global_vars);
  tree global_vars_array_type = build_array_type (variable_info_type,
						  global_vars_num_index_type);
  TYPE_ARTIFICIAL (global_vars_array_type) = 1;

  vec<constructor_elt, va_gc> *global_vars_vec = NULL;

  for (hash_table <hsa_noop_symbol_hasher>::iterator it
       = hsa_global_variable_symbols->begin ();
       it != hsa_global_variable_symbols->end (); ++it)
    {
      unsigned len = strlen ((*it)->m_name);
      char *copy = XNEWVEC (char, len + 2);
      copy[0] = '&';
      memcpy (copy + 1, (*it)->m_name, len);
      copy[len + 1] = '\0';
      len++;
      hsa_sanitize_name (copy);

      tree var_name = build_string (len, copy);
      TREE_TYPE (var_name)
	= build_array_type (char_type_node, build_index_type (size_int (len)));
      free (copy);

      vec<constructor_elt, va_gc> *variable_info_vec = NULL;
      CONSTRUCTOR_APPEND_ELT (variable_info_vec, NULL_TREE,
			      build1 (ADDR_EXPR,
				      build_pointer_type (TREE_TYPE (var_name)),
				      var_name));
      CONSTRUCTOR_APPEND_ELT (variable_info_vec, NULL_TREE,
			      build_fold_addr_expr ((*it)->m_decl));

      tree variable_info_ctor = build_constructor (variable_info_type,
						   variable_info_vec);

      CONSTRUCTOR_APPEND_ELT (global_vars_vec, NULL_TREE,
			      variable_info_ctor);
    }

  tree global_vars_ctor = build_constructor (global_vars_array_type,
					     global_vars_vec);

  char tmp_name[64];
  ASM_GENERATE_INTERNAL_LABEL (tmp_name, "__hsa_global_variables", 1);
  tree global_vars_table = build_decl (UNKNOWN_LOCATION, VAR_DECL,
					   get_identifier (tmp_name),
					   global_vars_array_type);
  TREE_STATIC (global_vars_table) = 1;
  TREE_READONLY (global_vars_table) = 1;
  TREE_PUBLIC (global_vars_table) = 0;
  DECL_ARTIFICIAL (global_vars_table) = 1;
  DECL_IGNORED_P (global_vars_table) = 1;
  DECL_EXTERNAL (global_vars_table) = 0;
  TREE_CONSTANT (global_vars_table) = 1;
  DECL_INITIAL (global_vars_table) = global_vars_ctor;
  varpool_node::finalize_decl (global_vars_table);

  return global_vars_table;
}

/* Create __hsa_host_functions and __hsa_kernels that contain
   all informations consumed by libgomp to register all kernels
   in the BRIG binary.  */

static void
hsa_output_kernels (tree *host_func_table, tree *kernels)
{
  unsigned map_count = hsa_get_number_decl_kernel_mappings ();

  tree int_num_of_kernels;
  int_num_of_kernels = build_int_cst (uint32_type_node, map_count);
  tree kernel_num_index_type = build_index_type (int_num_of_kernels);
  tree host_functions_array_type = build_array_type (ptr_type_node,
						     kernel_num_index_type);
  TYPE_ARTIFICIAL (host_functions_array_type) = 1;

  vec<constructor_elt, va_gc> *host_functions_vec = NULL;
  for (unsigned i = 0; i < map_count; ++i)
    {
      tree decl = hsa_get_decl_kernel_mapping_decl (i);
      tree host_fn = build_fold_addr_expr (hsa_get_host_function (decl));
      CONSTRUCTOR_APPEND_ELT (host_functions_vec, NULL_TREE, host_fn);
    }
  tree host_functions_ctor = build_constructor (host_functions_array_type,
						host_functions_vec);
  char tmp_name[64];
  ASM_GENERATE_INTERNAL_LABEL (tmp_name, "__hsa_host_functions", 1);
  tree hsa_host_func_table = build_decl (UNKNOWN_LOCATION, VAR_DECL,
					 get_identifier (tmp_name),
					 host_functions_array_type);
  TREE_STATIC (hsa_host_func_table) = 1;
  TREE_READONLY (hsa_host_func_table) = 1;
  TREE_PUBLIC (hsa_host_func_table) = 0;
  DECL_ARTIFICIAL (hsa_host_func_table) = 1;
  DECL_IGNORED_P (hsa_host_func_table) = 1;
  DECL_EXTERNAL (hsa_host_func_table) = 0;
  TREE_CONSTANT (hsa_host_func_table) = 1;
  DECL_INITIAL (hsa_host_func_table) = host_functions_ctor;
  varpool_node::finalize_decl (hsa_host_func_table);
  *host_func_table = hsa_host_func_table;

  /* Following code emits list of kernel_info structures.  */

  tree kernel_info_type = make_node (RECORD_TYPE);
  tree id_f1 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			   get_identifier ("name"), ptr_type_node);
  DECL_CHAIN (id_f1) = NULL_TREE;
  tree id_f2 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			   get_identifier ("omp_data_size"),
			   unsigned_type_node);
  DECL_CHAIN (id_f2) = id_f1;
  tree id_f3 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			   get_identifier ("gridified_kernel_p"),
			   boolean_type_node);
  DECL_CHAIN (id_f3) = id_f2;
  tree id_f4 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			   get_identifier ("kernel_dependencies_count"),
			   unsigned_type_node);
  DECL_CHAIN (id_f4) = id_f3;
  tree id_f5 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			   get_identifier ("kernel_dependencies"),
			   build_pointer_type (build_pointer_type
					       (char_type_node)));
  DECL_CHAIN (id_f5) = id_f4;
  finish_builtin_struct (kernel_info_type, "__hsa_kernel_info", id_f5,
			 NULL_TREE);

  int_num_of_kernels = build_int_cstu (uint32_type_node, map_count);
  tree kernel_info_vector_type
    = build_array_type (kernel_info_type,
			build_index_type (int_num_of_kernels));
  TYPE_ARTIFICIAL (kernel_info_vector_type) = 1;

  vec<constructor_elt, va_gc> *kernel_info_vector_vec = NULL;
  tree kernel_dependencies_vector_type = NULL;

  for (unsigned i = 0; i < map_count; ++i)
    {
      tree kernel = hsa_get_decl_kernel_mapping_decl (i);
      char *name = hsa_get_decl_kernel_mapping_name (i);
      unsigned len = strlen (name);
      char *copy = XNEWVEC (char, len + 2);
      copy[0] = '&';
      memcpy (copy + 1, name, len);
      copy[len + 1] = '\0';
      len++;

      tree kern_name = build_string (len, copy);
      TREE_TYPE (kern_name)
	= build_array_type (char_type_node, build_index_type (size_int (len)));
      free (copy);

      unsigned omp_size = hsa_get_decl_kernel_mapping_omp_size (i);
      tree omp_data_size = build_int_cstu (unsigned_type_node, omp_size);
      bool gridified_kernel_p = hsa_get_decl_kernel_mapping_gridified (i);
      tree gridified_kernel_p_tree = build_int_cstu (boolean_type_node,
						     gridified_kernel_p);
      unsigned count = 0;

      kernel_dependencies_vector_type
	= build_array_type (build_pointer_type (char_type_node),
			    build_index_type (size_int (0)));

      vec<constructor_elt, va_gc> *kernel_dependencies_vec = NULL;
      if (hsa_decl_kernel_dependencies)
	{
	  vec<const char *> **slot;
	  slot = hsa_decl_kernel_dependencies->get (kernel);
	  if (slot)
	    {
	      vec <const char *> *dependencies = *slot;
	      count = dependencies->length ();

	      kernel_dependencies_vector_type
		= build_array_type (build_pointer_type (char_type_node),
				    build_index_type (size_int (count)));
	      TYPE_ARTIFICIAL (kernel_dependencies_vector_type) = 1;

	      for (unsigned j = 0; j < count; j++)
		{
		  const char *d = (*dependencies)[j];
		  len = strlen (d);
		  tree dependency_name = build_string (len, d);
		  TREE_TYPE (dependency_name)
		    = build_array_type (char_type_node,
					build_index_type (size_int (len)));

		  CONSTRUCTOR_APPEND_ELT
		    (kernel_dependencies_vec, NULL_TREE,
		     build1 (ADDR_EXPR,
			     build_pointer_type (TREE_TYPE (dependency_name)),
			     dependency_name));
		}
	    }
	}

      tree dependencies_count = build_int_cstu (unsigned_type_node, count);

      vec<constructor_elt, va_gc> *kernel_info_vec = NULL;
      CONSTRUCTOR_APPEND_ELT (kernel_info_vec, NULL_TREE,
			      build1 (ADDR_EXPR,
				      build_pointer_type (TREE_TYPE
							  (kern_name)),
				      kern_name));
      CONSTRUCTOR_APPEND_ELT (kernel_info_vec, NULL_TREE, omp_data_size);
      CONSTRUCTOR_APPEND_ELT (kernel_info_vec, NULL_TREE,
			      gridified_kernel_p_tree);
      CONSTRUCTOR_APPEND_ELT (kernel_info_vec, NULL_TREE, dependencies_count);

      if (count > 0)
	{
	  ASM_GENERATE_INTERNAL_LABEL (tmp_name, "__hsa_dependencies_list", i);
	  tree dependencies_list = build_decl (UNKNOWN_LOCATION, VAR_DECL,
					       get_identifier (tmp_name),
					       kernel_dependencies_vector_type);

	  TREE_STATIC (dependencies_list) = 1;
	  TREE_READONLY (dependencies_list) = 1;
	  TREE_PUBLIC (dependencies_list) = 0;
	  DECL_ARTIFICIAL (dependencies_list) = 1;
	  DECL_IGNORED_P (dependencies_list) = 1;
	  DECL_EXTERNAL (dependencies_list) = 0;
	  TREE_CONSTANT (dependencies_list) = 1;
	  DECL_INITIAL (dependencies_list)
	    = build_constructor (kernel_dependencies_vector_type,
				 kernel_dependencies_vec);
	  varpool_node::finalize_decl (dependencies_list);

	  CONSTRUCTOR_APPEND_ELT (kernel_info_vec, NULL_TREE,
				  build1 (ADDR_EXPR,
					  build_pointer_type
					    (TREE_TYPE (dependencies_list)),
					  dependencies_list));
	}
      else
	CONSTRUCTOR_APPEND_ELT (kernel_info_vec, NULL_TREE, null_pointer_node);

      tree kernel_info_ctor = build_constructor (kernel_info_type,
						 kernel_info_vec);

      CONSTRUCTOR_APPEND_ELT (kernel_info_vector_vec, NULL_TREE,
			      kernel_info_ctor);
    }

  ASM_GENERATE_INTERNAL_LABEL (tmp_name, "__hsa_kernels", 1);
  tree hsa_kernels = build_decl (UNKNOWN_LOCATION, VAR_DECL,
				 get_identifier (tmp_name),
				 kernel_info_vector_type);

  TREE_STATIC (hsa_kernels) = 1;
  TREE_READONLY (hsa_kernels) = 1;
  TREE_PUBLIC (hsa_kernels) = 0;
  DECL_ARTIFICIAL (hsa_kernels) = 1;
  DECL_IGNORED_P (hsa_kernels) = 1;
  DECL_EXTERNAL (hsa_kernels) = 0;
  TREE_CONSTANT (hsa_kernels) = 1;
  DECL_INITIAL (hsa_kernels) = build_constructor (kernel_info_vector_type,
						  kernel_info_vector_vec);
  varpool_node::finalize_decl (hsa_kernels);
  *kernels = hsa_kernels;
}

/* Create a static constructor that will register out brig stuff with
   libgomp.  */

static void
hsa_output_libgomp_mapping (tree brig_decl)
{
  unsigned kernel_count = hsa_get_number_decl_kernel_mappings ();
  unsigned global_variable_count = hsa_global_variable_symbols->elements ();

  tree kernels;
  tree host_func_table;

  hsa_output_kernels (&host_func_table, &kernels);
  tree global_vars = hsa_output_global_variables ();

  tree hsa_image_desc_type = make_node (RECORD_TYPE);
  tree id_f1 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			   get_identifier ("brig_module"), ptr_type_node);
  DECL_CHAIN (id_f1) = NULL_TREE;
  tree id_f2 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			   get_identifier ("kernel_count"),
			   unsigned_type_node);

  DECL_CHAIN (id_f2) = id_f1;
  tree id_f3 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			   get_identifier ("hsa_kernel_infos"),
			   ptr_type_node);
  DECL_CHAIN (id_f3) = id_f2;
  tree id_f4 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			   get_identifier ("global_variable_count"),
			   unsigned_type_node);
  DECL_CHAIN (id_f4) = id_f3;
  tree id_f5 = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			   get_identifier ("hsa_global_variable_infos"),
			   ptr_type_node);
  DECL_CHAIN (id_f5) = id_f4;
  finish_builtin_struct (hsa_image_desc_type, "__hsa_image_desc", id_f5,
			 NULL_TREE);
  TYPE_ARTIFICIAL (hsa_image_desc_type) = 1;

  vec<constructor_elt, va_gc> *img_desc_vec = NULL;
  CONSTRUCTOR_APPEND_ELT (img_desc_vec, NULL_TREE,
			  build_fold_addr_expr (brig_decl));
  CONSTRUCTOR_APPEND_ELT (img_desc_vec, NULL_TREE,
			  build_int_cstu (unsigned_type_node, kernel_count));
  CONSTRUCTOR_APPEND_ELT (img_desc_vec, NULL_TREE,
			  build1 (ADDR_EXPR,
				  build_pointer_type (TREE_TYPE (kernels)),
				  kernels));
  CONSTRUCTOR_APPEND_ELT (img_desc_vec, NULL_TREE,
			  build_int_cstu (unsigned_type_node,
					  global_variable_count));
  CONSTRUCTOR_APPEND_ELT (img_desc_vec, NULL_TREE,
			  build1 (ADDR_EXPR,
				  build_pointer_type (TREE_TYPE (global_vars)),
				  global_vars));

  tree img_desc_ctor = build_constructor (hsa_image_desc_type, img_desc_vec);

  char tmp_name[64];
  ASM_GENERATE_INTERNAL_LABEL (tmp_name, "__hsa_img_descriptor", 1);
  tree hsa_img_descriptor = build_decl (UNKNOWN_LOCATION, VAR_DECL,
					get_identifier (tmp_name),
					hsa_image_desc_type);
  TREE_STATIC (hsa_img_descriptor) = 1;
  TREE_READONLY (hsa_img_descriptor) = 1;
  TREE_PUBLIC (hsa_img_descriptor) = 0;
  DECL_ARTIFICIAL (hsa_img_descriptor) = 1;
  DECL_IGNORED_P (hsa_img_descriptor) = 1;
  DECL_EXTERNAL (hsa_img_descriptor) = 0;
  TREE_CONSTANT (hsa_img_descriptor) = 1;
  DECL_INITIAL (hsa_img_descriptor) = img_desc_ctor;
  varpool_node::finalize_decl (hsa_img_descriptor);

  /* Construct the "host_table" libgomp expects.  */
  tree index_type = build_index_type (build_int_cst (integer_type_node, 4));
  tree libgomp_host_table_type = build_array_type (ptr_type_node, index_type);
  TYPE_ARTIFICIAL (libgomp_host_table_type) = 1;
  vec<constructor_elt, va_gc> *libgomp_host_table_vec = NULL;
  tree host_func_table_addr = build_fold_addr_expr (host_func_table);
  CONSTRUCTOR_APPEND_ELT (libgomp_host_table_vec, NULL_TREE,
			  host_func_table_addr);
  offset_int func_table_size
    = wi::to_offset (TYPE_SIZE_UNIT (ptr_type_node)) * kernel_count;
  CONSTRUCTOR_APPEND_ELT (libgomp_host_table_vec, NULL_TREE,
			  fold_build2 (POINTER_PLUS_EXPR,
				       TREE_TYPE (host_func_table_addr),
				       host_func_table_addr,
				       build_int_cst (size_type_node,
						      func_table_size.to_uhwi
						      ())));
  CONSTRUCTOR_APPEND_ELT (libgomp_host_table_vec, NULL_TREE, null_pointer_node);
  CONSTRUCTOR_APPEND_ELT (libgomp_host_table_vec, NULL_TREE, null_pointer_node);
  tree libgomp_host_table_ctor = build_constructor (libgomp_host_table_type,
						    libgomp_host_table_vec);
  ASM_GENERATE_INTERNAL_LABEL (tmp_name, "__hsa_libgomp_host_table", 1);
  tree hsa_libgomp_host_table = build_decl (UNKNOWN_LOCATION, VAR_DECL,
					    get_identifier (tmp_name),
					    libgomp_host_table_type);

  TREE_STATIC (hsa_libgomp_host_table) = 1;
  TREE_READONLY (hsa_libgomp_host_table) = 1;
  TREE_PUBLIC (hsa_libgomp_host_table) = 0;
  DECL_ARTIFICIAL (hsa_libgomp_host_table) = 1;
  DECL_IGNORED_P (hsa_libgomp_host_table) = 1;
  DECL_EXTERNAL (hsa_libgomp_host_table) = 0;
  TREE_CONSTANT (hsa_libgomp_host_table) = 1;
  DECL_INITIAL (hsa_libgomp_host_table) = libgomp_host_table_ctor;
  varpool_node::finalize_decl (hsa_libgomp_host_table);

  /* Generate an initializer with a call to the registration routine.  */

  tree offload_register
    = builtin_decl_explicit (BUILT_IN_GOMP_OFFLOAD_REGISTER);
  gcc_checking_assert (offload_register);

  append_to_statement_list
    (build_call_expr (offload_register, 4,
		      build_int_cstu (unsigned_type_node,
				      GOMP_VERSION_PACK (GOMP_VERSION,
							 GOMP_VERSION_HSA)),
		      build_fold_addr_expr (hsa_libgomp_host_table),
		      build_int_cst (integer_type_node, GOMP_DEVICE_HSA),
		      build_fold_addr_expr (hsa_img_descriptor)),
     &hsa_cdtor_statements[0]);

  cgraph_build_static_cdtor ('I', hsa_cdtor_statements[0],
			     DEFAULT_INIT_PRIORITY);

  tree offload_unregister
    = builtin_decl_explicit (BUILT_IN_GOMP_OFFLOAD_UNREGISTER);
  gcc_checking_assert (offload_unregister);

  append_to_statement_list
    (build_call_expr (offload_unregister, 4,
		      build_int_cstu (unsigned_type_node,
				      GOMP_VERSION_PACK (GOMP_VERSION,
							 GOMP_VERSION_HSA)),
		      build_fold_addr_expr (hsa_libgomp_host_table),
		      build_int_cst (integer_type_node, GOMP_DEVICE_HSA),
		      build_fold_addr_expr (hsa_img_descriptor)),
     &hsa_cdtor_statements[1]);
  cgraph_build_static_cdtor ('D', hsa_cdtor_statements[1],
			     DEFAULT_INIT_PRIORITY);
}

/* Emit the brig module we have compiled to a section in the final assembly and
   also create a compile unit static constructor that will register the brig
   module with libgomp.  */

void
hsa_output_brig (void)
{
  section *saved_section;

  if (!brig_initialized)
    return;

  for (unsigned i = 0; i < function_call_linkage.length (); i++)
    {
      function_linkage_pair p = function_call_linkage[i];

      BrigCodeOffset32_t *func_offset = function_offsets->get (p.function_decl);
      gcc_assert (*func_offset);
      BrigOperandCodeRef *code_ref
	= (BrigOperandCodeRef *) (brig_operand.get_ptr_by_offset (p.offset));
      gcc_assert (code_ref->base.kind == BRIG_KIND_OPERAND_CODE_REF);
      code_ref->ref = lendian32 (*func_offset);
    }

  /* Iterate all function declarations and if we meet a function that should
     have module linkage and we are unable to emit HSAIL for the function,
     then change the linkage to program linkage.  Doing so, we will emit
     a valid BRIG image.  */
  if (hsa_failed_functions != NULL && emitted_declarations != NULL)
    for (hash_map <tree, BrigDirectiveExecutable *>::iterator it
	 = emitted_declarations->begin ();
	 it != emitted_declarations->end ();
	 ++it)
      {
	if (hsa_failed_functions->contains ((*it).first))
	  (*it).second->linkage = BRIG_LINKAGE_PROGRAM;
      }

  saved_section = in_section;

  switch_to_section (get_section (BRIG_ELF_SECTION_NAME, SECTION_NOTYPE, NULL));
  char tmp_name[64];
  ASM_GENERATE_INTERNAL_LABEL (tmp_name, BRIG_LABEL_STRING, 1);
  ASM_OUTPUT_LABEL (asm_out_file, tmp_name);
  tree brig_id = get_identifier (tmp_name);
  tree brig_decl = build_decl (UNKNOWN_LOCATION, VAR_DECL, brig_id,
			       char_type_node);
  SET_DECL_ASSEMBLER_NAME (brig_decl, brig_id);
  TREE_ADDRESSABLE (brig_decl) = 1;
  TREE_READONLY (brig_decl) = 1;
  DECL_ARTIFICIAL (brig_decl) = 1;
  DECL_IGNORED_P (brig_decl) = 1;
  TREE_STATIC (brig_decl) = 1;
  TREE_PUBLIC (brig_decl) = 0;
  TREE_USED (brig_decl) = 1;
  DECL_INITIAL (brig_decl) = brig_decl;
  TREE_ASM_WRITTEN (brig_decl) = 1;

  BrigModuleHeader module_header;
  memcpy (&module_header.identification, "HSA BRIG",
	  sizeof (module_header.identification));
  module_header.brigMajor = lendian32 (BRIG_VERSION_BRIG_MAJOR);
  module_header.brigMinor = lendian32 (BRIG_VERSION_BRIG_MINOR);
  uint64_t section_index[3];

  int data_padding, code_padding, operand_padding;
  data_padding = HSA_SECTION_ALIGNMENT
    - brig_data.total_size % HSA_SECTION_ALIGNMENT;
  code_padding = HSA_SECTION_ALIGNMENT
    - brig_code.total_size % HSA_SECTION_ALIGNMENT;
  operand_padding = HSA_SECTION_ALIGNMENT
    - brig_operand.total_size % HSA_SECTION_ALIGNMENT;

  uint64_t module_size = sizeof (module_header)
    + sizeof (section_index)
    + brig_data.total_size
    + data_padding
    + brig_code.total_size
    + code_padding
    + brig_operand.total_size
    + operand_padding;
  gcc_assert ((module_size % 16) == 0);
  module_header.byteCount = lendian64 (module_size);
  memset (&module_header.hash, 0, sizeof (module_header.hash));
  module_header.reserved = 0;
  module_header.sectionCount = lendian32 (3);
  module_header.sectionIndex = lendian64 (sizeof (module_header));
  assemble_string ((const char *) &module_header, sizeof (module_header));
  uint64_t off = sizeof (module_header) + sizeof (section_index);
  section_index[0] = lendian64 (off);
  off += brig_data.total_size + data_padding;
  section_index[1] = lendian64 (off);
  off += brig_code.total_size + code_padding;
  section_index[2] = lendian64 (off);
  assemble_string ((const char *) &section_index, sizeof (section_index));

  char padding[HSA_SECTION_ALIGNMENT];
  memset (padding, 0, sizeof (padding));

  brig_data.output ();
  assemble_string (padding, data_padding);
  brig_code.output ();
  assemble_string (padding, code_padding);
  brig_operand.output ();
  assemble_string (padding, operand_padding);

  if (saved_section)
    switch_to_section (saved_section);

  hsa_output_libgomp_mapping (brig_decl);

  hsa_free_decl_kernel_mapping ();
  brig_release_data ();
  hsa_deinit_compilation_unit_data ();

  delete emitted_declarations;
  emitted_declarations = NULL;
  delete function_offsets;
  function_offsets = NULL;
}
