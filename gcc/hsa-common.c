/* Implementation of commonly needed HSAIL related functions and methods.
   Copyright (C) 2013-2017 Free Software Foundation, Inc.
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
#include "is-a.h"
#include "hash-set.h"
#include "hash-map.h"
#include "vec.h"
#include "tree.h"
#include "dumpfile.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "alloc-pool.h"
#include "cgraph.h"
#include "print-tree.h"
#include "stringpool.h"
#include "symbol-summary.h"
#include "hsa-common.h"
#include "internal-fn.h"
#include "ctype.h"
#include "builtins.h"
#include "stringpool.h"
#include "attribs.h"

/* Structure containing intermediate HSA representation of the generated
   function.  */
class hsa_function_representation *hsa_cfun;

/* Element of the mapping vector between a host decl and an HSA function.  */

struct GTY(()) hsa_decl_function_map_element
{
  /* The decl of the host function.  */
  tree decl;
  /* Name of the HSA function in BRIG.  */
  char * GTY((skip)) name;
  /* Size of OMP data, if the kernel contains a kernel dispatch.  */
  unsigned omp_data_size;
  /* True if the function is a host-callable kernel.  */
  bool kernel_p;
  /* True if the function is a gridified kernel.  */
  bool gridified_kernel_p;
};

/* Mapping between decls and corresponding HSA kernels in this compilation
   unit.  */

static GTY (()) vec<hsa_decl_function_map_element, va_gc>
  *hsa_decl_function_mapping;

/* Mapping between decls and corresponding HSA kernels
   called by the function.  */
hash_map <tree, vec <const char *> *> *hsa_decl_function_dependencies;

/* Hash function to lookup a symbol for a decl.  */
hash_table <hsa_noop_symbol_hasher> *hsa_global_variable_symbols;

/* HSA summaries.  */
hsa_summary_t *hsa_summaries = NULL;

/* HSA number of threads.  */
hsa_symbol *hsa_num_threads = NULL;

/* HSA function that cannot be expanded to HSAIL.  */
hash_set <tree> *hsa_failed_functions = NULL;

/* True if compilation unit-wide data are already allocated and initialized.  */
static bool compilation_unit_data_initialized;

/* Return true if FNDECL represents an HSA-callable function.  */

bool
hsa_callable_function_p (tree fndecl)
{
  return (lookup_attribute ("omp declare target", DECL_ATTRIBUTES (fndecl))
	  && !lookup_attribute ("oacc function", DECL_ATTRIBUTES (fndecl)));
}

/* Allocate HSA structures that are are used when dealing with different
   functions.  */

void
hsa_init_compilation_unit_data (void)
{
  if (compilation_unit_data_initialized)
    return;

  compilation_unit_data_initialized = true;

  hsa_global_variable_symbols = new hash_table <hsa_noop_symbol_hasher> (8);
  hsa_failed_functions = new hash_set <tree> ();
  hsa_emitted_internal_decls = new hash_table <hsa_internal_fn_hasher> (2);
}

/* Free data structures that are used when dealing with different
   functions.  */

void
hsa_deinit_compilation_unit_data (void)
{
  gcc_assert (compilation_unit_data_initialized);

  delete hsa_failed_functions;
  delete hsa_emitted_internal_decls;

  for (hash_table <hsa_noop_symbol_hasher>::iterator it
       = hsa_global_variable_symbols->begin ();
       it != hsa_global_variable_symbols->end ();
       ++it)
    {
      hsa_symbol *sym = *it;
      delete sym;
    }

  delete hsa_global_variable_symbols;

  if (hsa_num_threads)
    {
      delete hsa_num_threads;
      hsa_num_threads = NULL;
    }

  compilation_unit_data_initialized = false;
}

/* Return true if we are generating large HSA machine model.  */

bool
hsa_machine_large_p (void)
{
  /* FIXME: I suppose this is technically wrong but should work for me now.  */
  return (GET_MODE_BITSIZE (Pmode) == 64);
}

/* Return the HSA profile we are using.  */

bool
hsa_full_profile_p (void)
{
  return true;
}

/* Return true if a register in operand number OPNUM of instruction
   is an output.  False if it is an input.  */

bool
hsa_insn_basic::op_output_p (unsigned opnum)
{
  switch (m_opcode)
    {
    case HSA_OPCODE_PHI:
    case BRIG_OPCODE_CBR:
    case BRIG_OPCODE_SBR:
    case BRIG_OPCODE_ST:
    case BRIG_OPCODE_SIGNALNORET:
    case BRIG_OPCODE_DEBUGTRAP:
      /* FIXME: There are probably missing cases here, double check.  */
      return false;
    case BRIG_OPCODE_EXPAND:
      /* Example: expand_v4_b32_b128 (dest0, dest1, dest2, dest3), src0.  */
      return opnum < operand_count () - 1;
    default:
     return opnum == 0;
    }
}

/* Return true if OPCODE is an floating-point bit instruction opcode.  */

bool
hsa_opcode_floating_bit_insn_p (BrigOpcode16_t opcode)
{
  switch (opcode)
    {
    case BRIG_OPCODE_NEG:
    case BRIG_OPCODE_ABS:
    case BRIG_OPCODE_CLASS:
    case BRIG_OPCODE_COPYSIGN:
      return true;
    default:
      return false;
    }
}

/* Return the number of destination operands for this INSN.  */

unsigned
hsa_insn_basic::input_count ()
{
  switch (m_opcode)
    {
      default:
	return 1;

      case BRIG_OPCODE_NOP:
	return 0;

      case BRIG_OPCODE_EXPAND:
	return 2;

      case BRIG_OPCODE_LD:
	/* ld_v[234] not yet handled.  */
	return 1;

      case BRIG_OPCODE_ST:
	return 0;

      case BRIG_OPCODE_ATOMICNORET:
	return 0;

      case BRIG_OPCODE_SIGNAL:
	return 1;

      case BRIG_OPCODE_SIGNALNORET:
	return 0;

      case BRIG_OPCODE_MEMFENCE:
	return 0;

      case BRIG_OPCODE_RDIMAGE:
      case BRIG_OPCODE_LDIMAGE:
      case BRIG_OPCODE_STIMAGE:
      case BRIG_OPCODE_QUERYIMAGE:
      case BRIG_OPCODE_QUERYSAMPLER:
	sorry ("HSA image ops not handled");
	return 0;

      case BRIG_OPCODE_CBR:
      case BRIG_OPCODE_BR:
	return 0;

      case BRIG_OPCODE_SBR:
	return 0; /* ??? */

      case BRIG_OPCODE_WAVEBARRIER:
	return 0; /* ??? */

      case BRIG_OPCODE_BARRIER:
      case BRIG_OPCODE_ARRIVEFBAR:
      case BRIG_OPCODE_INITFBAR:
      case BRIG_OPCODE_JOINFBAR:
      case BRIG_OPCODE_LEAVEFBAR:
      case BRIG_OPCODE_RELEASEFBAR:
      case BRIG_OPCODE_WAITFBAR:
	return 0;

      case BRIG_OPCODE_LDF:
	return 1;

      case BRIG_OPCODE_ACTIVELANECOUNT:
      case BRIG_OPCODE_ACTIVELANEID:
      case BRIG_OPCODE_ACTIVELANEMASK:
      case BRIG_OPCODE_ACTIVELANEPERMUTE:
	return 1; /* ??? */

      case BRIG_OPCODE_CALL:
      case BRIG_OPCODE_SCALL:
      case BRIG_OPCODE_ICALL:
	return 0;

      case BRIG_OPCODE_RET:
	return 0;

      case BRIG_OPCODE_ALLOCA:
	return 1;

      case BRIG_OPCODE_CLEARDETECTEXCEPT:
	return 0;

      case BRIG_OPCODE_SETDETECTEXCEPT:
	return 0;

      case BRIG_OPCODE_PACKETCOMPLETIONSIG:
      case BRIG_OPCODE_PACKETID:
      case BRIG_OPCODE_CASQUEUEWRITEINDEX:
      case BRIG_OPCODE_LDQUEUEREADINDEX:
      case BRIG_OPCODE_LDQUEUEWRITEINDEX:
      case BRIG_OPCODE_STQUEUEREADINDEX:
      case BRIG_OPCODE_STQUEUEWRITEINDEX:
	return 1; /* ??? */

      case BRIG_OPCODE_ADDQUEUEWRITEINDEX:
	return 1;

      case BRIG_OPCODE_DEBUGTRAP:
	return 0;

      case BRIG_OPCODE_GROUPBASEPTR:
      case BRIG_OPCODE_KERNARGBASEPTR:
	return 1; /* ??? */

      case HSA_OPCODE_ARG_BLOCK:
	return 0;

      case BRIG_KIND_DIRECTIVE_COMMENT:
	return 0;
    }
}

/* Return the number of source operands for this INSN.  */

unsigned
hsa_insn_basic::num_used_ops ()
{
  gcc_checking_assert (input_count () <= operand_count ());

  return operand_count () - input_count ();
}

/* Set alignment to VALUE.  */

void
hsa_insn_mem::set_align (BrigAlignment8_t value)
{
  /* TODO: Perhaps remove this dump later on:  */
  if (dump_file && (dump_flags & TDF_DETAILS) && value < m_align)
    {
      fprintf (dump_file, "Decreasing alignment to %u in instruction ", value);
      dump_hsa_insn (dump_file, this);
    }
  m_align = value;
}

/* Return size of HSA type T in bits.  */

unsigned
hsa_type_bit_size (BrigType16_t t)
{
  switch (t)
    {
    case BRIG_TYPE_B1:
      return 1;

    case BRIG_TYPE_U8:
    case BRIG_TYPE_S8:
    case BRIG_TYPE_B8:
      return 8;

    case BRIG_TYPE_U16:
    case BRIG_TYPE_S16:
    case BRIG_TYPE_B16:
    case BRIG_TYPE_F16:
      return 16;

    case BRIG_TYPE_U32:
    case BRIG_TYPE_S32:
    case BRIG_TYPE_B32:
    case BRIG_TYPE_F32:
    case BRIG_TYPE_U8X4:
    case BRIG_TYPE_U16X2:
    case BRIG_TYPE_S8X4:
    case BRIG_TYPE_S16X2:
    case BRIG_TYPE_F16X2:
      return 32;

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

      return 64;

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
      return 128;

    default:
      gcc_assert (hsa_seen_error ());
      return t;
    }
}

/* Return BRIG bit-type with BITSIZE length.  */

BrigType16_t
hsa_bittype_for_bitsize (unsigned bitsize)
{
  switch (bitsize)
    {
    case 1:
      return BRIG_TYPE_B1;
    case 8:
      return BRIG_TYPE_B8;
    case 16:
      return BRIG_TYPE_B16;
    case 32:
      return BRIG_TYPE_B32;
    case 64:
      return BRIG_TYPE_B64;
    case 128:
      return BRIG_TYPE_B128;
    default:
      gcc_unreachable ();
    }
}

/* Return BRIG unsigned int type with BITSIZE length.  */

BrigType16_t
hsa_uint_for_bitsize (unsigned bitsize)
{
  switch (bitsize)
    {
    case 8:
      return BRIG_TYPE_U8;
    case 16:
      return BRIG_TYPE_U16;
    case 32:
      return BRIG_TYPE_U32;
    case 64:
      return BRIG_TYPE_U64;
    default:
      gcc_unreachable ();
    }
}

/* Return BRIG float type with BITSIZE length.  */

BrigType16_t
hsa_float_for_bitsize (unsigned bitsize)
{
  switch (bitsize)
    {
    case 16:
      return BRIG_TYPE_F16;
    case 32:
      return BRIG_TYPE_F32;
    case 64:
      return BRIG_TYPE_F64;
    default:
      gcc_unreachable ();
    }
}

/* Return HSA bit-type with the same size as the type T.  */

BrigType16_t
hsa_bittype_for_type (BrigType16_t t)
{
  return hsa_bittype_for_bitsize (hsa_type_bit_size (t));
}

/* Return HSA unsigned integer type with the same size as the type T.  */

BrigType16_t
hsa_unsigned_type_for_type (BrigType16_t t)
{
  return hsa_uint_for_bitsize (hsa_type_bit_size (t));
}

/* Return true if TYPE is a packed HSA type.  */

bool
hsa_type_packed_p (BrigType16_t type)
{
  return (type & BRIG_TYPE_PACK_MASK) != BRIG_TYPE_PACK_NONE;
}

/* Return true if and only if TYPE is a floating point number type.  */

bool
hsa_type_float_p (BrigType16_t type)
{
  switch (type & BRIG_TYPE_BASE_MASK)
    {
    case BRIG_TYPE_F16:
    case BRIG_TYPE_F32:
    case BRIG_TYPE_F64:
      return true;
    default:
      return false;
    }
}

/* Return true if and only if TYPE is an integer number type.  */

bool
hsa_type_integer_p (BrigType16_t type)
{
  switch (type & BRIG_TYPE_BASE_MASK)
    {
    case BRIG_TYPE_U8:
    case BRIG_TYPE_U16:
    case BRIG_TYPE_U32:
    case BRIG_TYPE_U64:
    case BRIG_TYPE_S8:
    case BRIG_TYPE_S16:
    case BRIG_TYPE_S32:
    case BRIG_TYPE_S64:
      return true;
    default:
      return false;
    }
}

/* Return true if and only if TYPE is an bit-type.  */

bool
hsa_btype_p (BrigType16_t type)
{
  switch (type & BRIG_TYPE_BASE_MASK)
    {
    case BRIG_TYPE_B8:
    case BRIG_TYPE_B16:
    case BRIG_TYPE_B32:
    case BRIG_TYPE_B64:
    case BRIG_TYPE_B128:
      return true;
    default:
      return false;
    }
}


/* Return HSA alignment encoding alignment to N bits.  */

BrigAlignment8_t
hsa_alignment_encoding (unsigned n)
{
  gcc_assert (n >= 8 && !(n & (n - 1)));
  if (n >= 256)
    return BRIG_ALIGNMENT_32;

  switch (n)
    {
    case 8:
      return BRIG_ALIGNMENT_1;
    case 16:
      return BRIG_ALIGNMENT_2;
    case 32:
      return BRIG_ALIGNMENT_4;
    case 64:
      return BRIG_ALIGNMENT_8;
    case 128:
      return BRIG_ALIGNMENT_16;
    default:
      gcc_unreachable ();
    }
}

/* Return HSA alignment encoding alignment of T got
   by get_object_alignment.  */

BrigAlignment8_t
hsa_object_alignment (tree t)
{
  return hsa_alignment_encoding (get_object_alignment (t));
}

/* Return byte alignment for given BrigAlignment8_t value.  */

unsigned
hsa_byte_alignment (BrigAlignment8_t alignment)
{
  gcc_assert (alignment != BRIG_ALIGNMENT_NONE);

  return 1 << (alignment - 1);
}

/* Return natural alignment of HSA TYPE.  */

BrigAlignment8_t
hsa_natural_alignment (BrigType16_t type)
{
  return hsa_alignment_encoding (hsa_type_bit_size (type & ~BRIG_TYPE_ARRAY));
}

/* Call the correct destructor of a HSA instruction.  */

void
hsa_destroy_insn (hsa_insn_basic *insn)
{
  if (hsa_insn_phi *phi = dyn_cast <hsa_insn_phi *> (insn))
    phi->~hsa_insn_phi ();
  else if (hsa_insn_cbr *br = dyn_cast <hsa_insn_cbr *> (insn))
    br->~hsa_insn_cbr ();
  else if (hsa_insn_cmp *cmp = dyn_cast <hsa_insn_cmp *> (insn))
    cmp->~hsa_insn_cmp ();
  else if (hsa_insn_mem *mem = dyn_cast <hsa_insn_mem *> (insn))
    mem->~hsa_insn_mem ();
  else if (hsa_insn_atomic *atomic = dyn_cast <hsa_insn_atomic *> (insn))
    atomic->~hsa_insn_atomic ();
  else if (hsa_insn_seg *seg = dyn_cast <hsa_insn_seg *> (insn))
    seg->~hsa_insn_seg ();
  else if (hsa_insn_call *call = dyn_cast <hsa_insn_call *> (insn))
    call->~hsa_insn_call ();
  else if (hsa_insn_arg_block *block = dyn_cast <hsa_insn_arg_block *> (insn))
    block->~hsa_insn_arg_block ();
  else if (hsa_insn_sbr *sbr = dyn_cast <hsa_insn_sbr *> (insn))
    sbr->~hsa_insn_sbr ();
  else if (hsa_insn_br *br = dyn_cast <hsa_insn_br *> (insn))
    br->~hsa_insn_br ();
  else if (hsa_insn_comment *comment = dyn_cast <hsa_insn_comment *> (insn))
    comment->~hsa_insn_comment ();
  else
    insn->~hsa_insn_basic ();
}

/* Call the correct destructor of a HSA operand.  */

void
hsa_destroy_operand (hsa_op_base *op)
{
  if (hsa_op_code_list *list = dyn_cast <hsa_op_code_list *> (op))
    list->~hsa_op_code_list ();
  else if (hsa_op_operand_list *list = dyn_cast <hsa_op_operand_list *> (op))
    list->~hsa_op_operand_list ();
  else if (hsa_op_reg *reg = dyn_cast <hsa_op_reg *> (op))
    reg->~hsa_op_reg ();
  else if (hsa_op_immed *immed = dyn_cast <hsa_op_immed *> (op))
    immed->~hsa_op_immed ();
  else
    op->~hsa_op_base ();
}

/* Create a mapping between the original function DECL and HSA function
   named NAME.  */

void
hsa_add_function_decl_mapping (tree decl, char *name, unsigned omp_data_size,
			       struct hsa_function_summary *s)
{
  hsa_decl_function_map_element dkm;
  dkm.decl = decl;
  dkm.name = name;
  dkm.omp_data_size = omp_data_size;
  dkm.kernel_p = s->m_kind == HSA_KERNEL;
  dkm.gridified_kernel_p = s->m_gridified_kernel_p;
  vec_safe_push (hsa_decl_function_mapping, dkm);
}

/* Return the number of function decl name mappings.  */

unsigned
hsa_get_number_decl_function_mappings (void)
{
  return vec_safe_length (hsa_decl_function_mapping);
}

/* Return the decl in the Ith function decl name mapping.  */

tree
hsa_get_decl_function_mapping_decl (unsigned i)
{
  return (*hsa_decl_function_mapping)[i].decl;
}

/* Return the name in the Ith function decl name mapping.  */

char *
hsa_get_decl_function_mapping_name (unsigned i)
{
  return (*hsa_decl_function_mapping)[i].name;
}

/* Return maximum OMP size for function decl name mapping.  */

unsigned
hsa_get_decl_kernel_mapping_omp_size (unsigned i)
{
  return (*hsa_decl_function_mapping)[i].omp_data_size;
}

/* Return if the function is a gridified kernel in decl name mapping.  */

bool
hsa_get_decl_function_mapping_gridified_p (unsigned i)
{
  return (*hsa_decl_function_mapping)[i].gridified_kernel_p;
}

/* Return true if the function is a host-callable kernel.  */

bool
hsa_get_decl_function_mapping_kernel_p (unsigned i)
{
  return (*hsa_decl_function_mapping)[i].kernel_p;
}

/* Free the mapping between original decls and function names.  */

void
hsa_free_decl_function_mapping (void)
{
  if (hsa_decl_function_mapping == NULL)
    return;

  for (unsigned i = 0; i < hsa_decl_function_mapping->length (); ++i)
    free ((*hsa_decl_function_mapping)[i].name);
  ggc_free (hsa_decl_function_mapping);
}

/* Add new function dependency.  */

void
hsa_add_function_dependency (tree caller, const char *called_function)
{
  if (hsa_decl_function_dependencies == NULL)
    hsa_decl_function_dependencies = new hash_map<tree, vec<const char *> *> ();

  vec <const char *> *s = NULL;
  vec <const char *> **slot = hsa_decl_function_dependencies->get (caller);
  if (slot == NULL)
    {
      s = new vec <const char *> ();
      hsa_decl_function_dependencies->put (caller, s);
    }
  else
    s = *slot;

  s->safe_push (called_function);
}

/* Expansion to HSA needs a few gc roots to hold types, constructors etc.  In
   order to minimize the number of GTY roots, we'll root them all in the
   following array.  The individual elements should only be accessed by the
   very simple getters (of a pointer-to-tree) below.  */

static GTY(()) tree hsa_tree_gt_roots[3];

tree *
hsa_get_ctor_statements (void)
{
  return &hsa_tree_gt_roots[0];
}

tree *
hsa_get_dtor_statements (void)
{
  return &hsa_tree_gt_roots[1];
}

tree *
hsa_get_kernel_dispatch_type (void)
{
  return &hsa_tree_gt_roots[2];
}

/* Modify the name P in-place so that it is a valid HSA identifier.  */

void
hsa_sanitize_name (char *p)
{
  for (; *p; p++)
    if (*p == '.' || *p == '-')
      *p = '_';
}

/* Clone the name P, set trailing ampersand and sanitize the name.  */

char *
hsa_brig_function_name (const char *p)
{
  unsigned len = strlen (p);
  char *buf = XNEWVEC (char, len + 2);

  buf[0] = '&';
  buf[len + 1] = '\0';
  memcpy (buf + 1, p, len);

  hsa_sanitize_name (buf);
  return buf;
}

/* Add a flatten attribute and disable vectorization for gpu implementation
   function decl GDECL.  */

void hsa_summary_t::process_gpu_implementation_attributes (tree gdecl)
{
  DECL_ATTRIBUTES (gdecl)
    = tree_cons (get_identifier ("flatten"), NULL_TREE,
		 DECL_ATTRIBUTES (gdecl));

  tree fn_opts = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (gdecl);
  if (fn_opts == NULL_TREE)
    fn_opts = optimization_default_node;
  fn_opts = copy_node (fn_opts);
  TREE_OPTIMIZATION (fn_opts)->x_flag_tree_loop_vectorize = false;
  TREE_OPTIMIZATION (fn_opts)->x_flag_tree_slp_vectorize = false;
  DECL_FUNCTION_SPECIFIC_OPTIMIZATION (gdecl) = fn_opts;
}

void
hsa_summary_t::link_functions (cgraph_node *gpu, cgraph_node *host,
			       hsa_function_kind kind, bool gridified_kernel_p)
{
  hsa_function_summary *gpu_summary = get (gpu);
  hsa_function_summary *host_summary = get (host);

  gpu_summary->m_kind = kind;
  host_summary->m_kind = kind;

  gpu_summary->m_hsa_implementation_p = true;
  host_summary->m_hsa_implementation_p = false;

  gpu_summary->m_gridified_kernel_p = gridified_kernel_p;
  host_summary->m_gridified_kernel_p = gridified_kernel_p;

  gpu_summary->m_bound_function = host;
  host_summary->m_bound_function = gpu;

  process_gpu_implementation_attributes (gpu->decl);

  /* Create reference between an HSA function and a corresponding host
     implementation to quarantee LTO streaming to a same LTRANS.  */
  gpu->create_reference (host, IPA_REF_ADDR);
}

void
hsa_summary_t::mark_hsa_only_implementation (cgraph_node *node,
					     hsa_function_kind kind)
{
  hsa_function_summary *gpu_summary = get (node);
  gpu_summary->m_kind = kind;
  gpu_summary->m_hsa_implementation_p = true;
  gcc_assert (!gpu_summary->m_bound_function);
  process_gpu_implementation_attributes (node->decl);
}

/* Add a HOST function to HSA summaries.  */

void
hsa_register_function (cgraph_node *host, bool kernel_p)
{
  if (hsa_summaries == NULL)
    hsa_summaries = new hsa_summary_t (symtab);
  hsa_function_summary *s = hsa_summaries->get (host);
  s->m_kind = kernel_p ? HSA_KERNEL : HSA_FUNCTION;
}

/* Add a pair of functions to HSA summaries.  GPU is an HSA implementation of
   a HOST function.  */

void
hsa_register_function (cgraph_node *gpu, cgraph_node *host, bool kernel_p)
{
  if (hsa_summaries == NULL)
    hsa_summaries = new hsa_summary_t (symtab);
  if (kernel_p)
    hsa_summaries->link_functions (gpu, host, HSA_KERNEL, true);
  else
    hsa_summaries->link_functions (gpu, host, HSA_FUNCTION, true);
}

/* Return true if expansion of the current HSA function has already failed.  */

bool
hsa_seen_error (void)
{
  return hsa_cfun->m_seen_error;
}

/* Mark current HSA function as failed.  */

void
hsa_fail_cfun (void)
{
  hsa_failed_functions->add (hsa_cfun->m_decl);
  hsa_cfun->m_seen_error = true;
}

char *
hsa_internal_fn::name ()
{
  char *name = xstrdup (internal_fn_name (m_fn));
  for (char *ptr = name; *ptr; ptr++)
    *ptr = TOLOWER (*ptr);

  const char *suffix = NULL;
  if (m_type_bit_size == 32)
    suffix = "f";

  if (suffix)
    {
      char *name2 = concat (name, suffix, NULL);
      free (name);
      name = name2;
    }

  hsa_sanitize_name (name);
  return name;
}

unsigned
hsa_internal_fn::get_arity ()
{
  switch (m_fn)
    {
    case IFN_ACOS:
    case IFN_ASIN:
    case IFN_ATAN:
    case IFN_COS:
    case IFN_EXP:
    case IFN_EXP10:
    case IFN_EXP2:
    case IFN_EXPM1:
    case IFN_LOG:
    case IFN_LOG10:
    case IFN_LOG1P:
    case IFN_LOG2:
    case IFN_LOGB:
    case IFN_SIGNIFICAND:
    case IFN_SIN:
    case IFN_SQRT:
    case IFN_TAN:
    case IFN_CEIL:
    case IFN_FLOOR:
    case IFN_NEARBYINT:
    case IFN_RINT:
    case IFN_ROUND:
    case IFN_TRUNC:
      return 1;
    case IFN_ATAN2:
    case IFN_COPYSIGN:
    case IFN_FMOD:
    case IFN_POW:
    case IFN_REMAINDER:
    case IFN_SCALB:
    case IFN_LDEXP:
      return 2;
    case IFN_CLRSB:
    case IFN_CLZ:
    case IFN_CTZ:
    case IFN_FFS:
    case IFN_PARITY:
    case IFN_POPCOUNT:
    default:
      /* As we produce sorry message for unknown internal functions,
	 reaching this label is definitely a bug.  */
      gcc_unreachable ();
    }
}

BrigType16_t
hsa_internal_fn::get_argument_type (int n)
{
  switch (m_fn)
    {
    case IFN_ACOS:
    case IFN_ASIN:
    case IFN_ATAN:
    case IFN_COS:
    case IFN_EXP:
    case IFN_EXP10:
    case IFN_EXP2:
    case IFN_EXPM1:
    case IFN_LOG:
    case IFN_LOG10:
    case IFN_LOG1P:
    case IFN_LOG2:
    case IFN_LOGB:
    case IFN_SIGNIFICAND:
    case IFN_SIN:
    case IFN_SQRT:
    case IFN_TAN:
    case IFN_CEIL:
    case IFN_FLOOR:
    case IFN_NEARBYINT:
    case IFN_RINT:
    case IFN_ROUND:
    case IFN_TRUNC:
    case IFN_ATAN2:
    case IFN_COPYSIGN:
    case IFN_FMOD:
    case IFN_POW:
    case IFN_REMAINDER:
    case IFN_SCALB:
      return hsa_float_for_bitsize (m_type_bit_size);
    case IFN_LDEXP:
      {
	if (n == -1 || n == 0)
	  return hsa_float_for_bitsize (m_type_bit_size);
	else
	  return BRIG_TYPE_S32;
      }
    default:
      /* As we produce sorry message for unknown internal functions,
	 reaching this label is definitely a bug.  */
      gcc_unreachable ();
    }
}

#include "gt-hsa-common.h"
