/* AddressSanitizer, a fast memory error detector.
   Copyright (C) 2012-2017 Free Software Foundation, Inc.
   Contributed by Kostya Serebryany <kcc@google.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "memmodel.h"
#include "tm_p.h"
#include "ssa.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "optabs.h"
#include "emit-rtl.h"
#include "cgraph.h"
#include "gimple-pretty-print.h"
#include "alias.h"
#include "fold-const.h"
#include "cfganal.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "varasm.h"
#include "stor-layout.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "dojump.h"
#include "explow.h"
#include "expr.h"
#include "output.h"
#include "langhooks.h"
#include "cfgloop.h"
#include "gimple-builder.h"
#include "gimple-fold.h"
#include "ubsan.h"
#include "params.h"
#include "builtins.h"
#include "fnmatch.h"
#include "tree-inline.h"

/* AddressSanitizer finds out-of-bounds and use-after-free bugs
   with <2x slowdown on average.

   The tool consists of two parts:
   instrumentation module (this file) and a run-time library.
   The instrumentation module adds a run-time check before every memory insn.
     For a 8- or 16- byte load accessing address X:
       ShadowAddr = (X >> 3) + Offset
       ShadowValue = *(char*)ShadowAddr;  // *(short*) for 16-byte access.
       if (ShadowValue)
	 __asan_report_load8(X);
     For a load of N bytes (N=1, 2 or 4) from address X:
       ShadowAddr = (X >> 3) + Offset
       ShadowValue = *(char*)ShadowAddr;
       if (ShadowValue)
	 if ((X & 7) + N - 1 > ShadowValue)
	   __asan_report_loadN(X);
   Stores are instrumented similarly, but using __asan_report_storeN functions.
   A call too __asan_init_vN() is inserted to the list of module CTORs.
   N is the version number of the AddressSanitizer API. The changes between the
   API versions are listed in libsanitizer/asan/asan_interface_internal.h.

   The run-time library redefines malloc (so that redzone are inserted around
   the allocated memory) and free (so that reuse of free-ed memory is delayed),
   provides __asan_report* and __asan_init_vN functions.

   Read more:
   http://code.google.com/p/address-sanitizer/wiki/AddressSanitizerAlgorithm

   The current implementation supports detection of out-of-bounds and
   use-after-free in the heap, on the stack and for global variables.

   [Protection of stack variables]

   To understand how detection of out-of-bounds and use-after-free works
   for stack variables, lets look at this example on x86_64 where the
   stack grows downward:

     int
     foo ()
     {
       char a[23] = {0};
       int b[2] = {0};

       a[5] = 1;
       b[1] = 2;

       return a[5] + b[1];
     }

   For this function, the stack protected by asan will be organized as
   follows, from the top of the stack to the bottom:

   Slot 1/ [red zone of 32 bytes called 'RIGHT RedZone']

   Slot 2/ [8 bytes of red zone, that adds up to the space of 'a' to make
	   the next slot be 32 bytes aligned; this one is called Partial
	   Redzone; this 32 bytes alignment is an asan constraint]

   Slot 3/ [24 bytes for variable 'a']

   Slot 4/ [red zone of 32 bytes called 'Middle RedZone']

   Slot 5/ [24 bytes of Partial Red Zone (similar to slot 2]

   Slot 6/ [8 bytes for variable 'b']

   Slot 7/ [32 bytes of Red Zone at the bottom of the stack, called
	    'LEFT RedZone']

   The 32 bytes of LEFT red zone at the bottom of the stack can be
   decomposed as such:

     1/ The first 8 bytes contain a magical asan number that is always
     0x41B58AB3.

     2/ The following 8 bytes contains a pointer to a string (to be
     parsed at runtime by the runtime asan library), which format is
     the following:

      "<function-name> <space> <num-of-variables-on-the-stack>
      (<32-bytes-aligned-offset-in-bytes-of-variable> <space>
      <length-of-var-in-bytes> ){n} "

	where '(...){n}' means the content inside the parenthesis occurs 'n'
	times, with 'n' being the number of variables on the stack.

     3/ The following 8 bytes contain the PC of the current function which
     will be used by the run-time library to print an error message.

     4/ The following 8 bytes are reserved for internal use by the run-time.

   The shadow memory for that stack layout is going to look like this:

     - content of shadow memory 8 bytes for slot 7: 0xF1F1F1F1.
       The F1 byte pattern is a magic number called
       ASAN_STACK_MAGIC_LEFT and is a way for the runtime to know that
       the memory for that shadow byte is part of a the LEFT red zone
       intended to seat at the bottom of the variables on the stack.

     - content of shadow memory 8 bytes for slots 6 and 5:
       0xF4F4F400.  The F4 byte pattern is a magic number
       called ASAN_STACK_MAGIC_PARTIAL.  It flags the fact that the
       memory region for this shadow byte is a PARTIAL red zone
       intended to pad a variable A, so that the slot following
       {A,padding} is 32 bytes aligned.

       Note that the fact that the least significant byte of this
       shadow memory content is 00 means that 8 bytes of its
       corresponding memory (which corresponds to the memory of
       variable 'b') is addressable.

     - content of shadow memory 8 bytes for slot 4: 0xF2F2F2F2.
       The F2 byte pattern is a magic number called
       ASAN_STACK_MAGIC_MIDDLE.  It flags the fact that the memory
       region for this shadow byte is a MIDDLE red zone intended to
       seat between two 32 aligned slots of {variable,padding}.

     - content of shadow memory 8 bytes for slot 3 and 2:
       0xF4000000.  This represents is the concatenation of
       variable 'a' and the partial red zone following it, like what we
       had for variable 'b'.  The least significant 3 bytes being 00
       means that the 3 bytes of variable 'a' are addressable.

     - content of shadow memory 8 bytes for slot 1: 0xF3F3F3F3.
       The F3 byte pattern is a magic number called
       ASAN_STACK_MAGIC_RIGHT.  It flags the fact that the memory
       region for this shadow byte is a RIGHT red zone intended to seat
       at the top of the variables of the stack.

   Note that the real variable layout is done in expand_used_vars in
   cfgexpand.c.  As far as Address Sanitizer is concerned, it lays out
   stack variables as well as the different red zones, emits some
   prologue code to populate the shadow memory as to poison (mark as
   non-accessible) the regions of the red zones and mark the regions of
   stack variables as accessible, and emit some epilogue code to
   un-poison (mark as accessible) the regions of red zones right before
   the function exits.

   [Protection of global variables]

   The basic idea is to insert a red zone between two global variables
   and install a constructor function that calls the asan runtime to do
   the populating of the relevant shadow memory regions at load time.

   So the global variables are laid out as to insert a red zone between
   them. The size of the red zones is so that each variable starts on a
   32 bytes boundary.

   Then a constructor function is installed so that, for each global
   variable, it calls the runtime asan library function
   __asan_register_globals_with an instance of this type:

     struct __asan_global
     {
       // Address of the beginning of the global variable.
       const void *__beg;

       // Initial size of the global variable.
       uptr __size;

       // Size of the global variable + size of the red zone.  This
       //   size is 32 bytes aligned.
       uptr __size_with_redzone;

       // Name of the global variable.
       const void *__name;

       // Name of the module where the global variable is declared.
       const void *__module_name;

       // 1 if it has dynamic initialization, 0 otherwise.
       uptr __has_dynamic_init;

       // A pointer to struct that contains source location, could be NULL.
       __asan_global_source_location *__location;
     }

   A destructor function that calls the runtime asan library function
   _asan_unregister_globals is also installed.  */

static unsigned HOST_WIDE_INT asan_shadow_offset_value;
static bool asan_shadow_offset_computed;
static vec<char *> sanitized_sections;
static tree last_alloca_addr;

/* Set of variable declarations that are going to be guarded by
   use-after-scope sanitizer.  */

static hash_set<tree> *asan_handled_variables = NULL;

hash_set <tree> *asan_used_labels = NULL;

/* Sets shadow offset to value in string VAL.  */

bool
set_asan_shadow_offset (const char *val)
{
  char *endp;

  errno = 0;
#ifdef HAVE_LONG_LONG
  asan_shadow_offset_value = strtoull (val, &endp, 0);
#else
  asan_shadow_offset_value = strtoul (val, &endp, 0);
#endif
  if (!(*val != '\0' && *endp == '\0' && errno == 0))
    return false;

  asan_shadow_offset_computed = true;

  return true;
}

/* Set list of user-defined sections that need to be sanitized.  */

void
set_sanitized_sections (const char *sections)
{
  char *pat;
  unsigned i;
  FOR_EACH_VEC_ELT (sanitized_sections, i, pat)
    free (pat);
  sanitized_sections.truncate (0);

  for (const char *s = sections; *s; )
    {
      const char *end;
      for (end = s; *end && *end != ','; ++end);
      size_t len = end - s;
      sanitized_sections.safe_push (xstrndup (s, len));
      s = *end ? end + 1 : end;
    }
}

bool
asan_mark_p (gimple *stmt, enum asan_mark_flags flag)
{
  return (gimple_call_internal_p (stmt, IFN_ASAN_MARK)
	  && tree_to_uhwi (gimple_call_arg (stmt, 0)) == flag);
}

bool
asan_sanitize_stack_p (void)
{
  return (sanitize_flags_p (SANITIZE_ADDRESS) && ASAN_STACK);
}

bool
asan_sanitize_allocas_p (void)
{
  return (asan_sanitize_stack_p () && ASAN_PROTECT_ALLOCAS);
}

/* Checks whether section SEC should be sanitized.  */

static bool
section_sanitized_p (const char *sec)
{
  char *pat;
  unsigned i;
  FOR_EACH_VEC_ELT (sanitized_sections, i, pat)
    if (fnmatch (pat, sec, FNM_PERIOD) == 0)
      return true;
  return false;
}

/* Returns Asan shadow offset.  */

static unsigned HOST_WIDE_INT
asan_shadow_offset ()
{
  if (!asan_shadow_offset_computed)
    {
      asan_shadow_offset_computed = true;
      asan_shadow_offset_value = targetm.asan_shadow_offset ();
    }
  return asan_shadow_offset_value;
}

alias_set_type asan_shadow_set = -1;

/* Pointer types to 1, 2 or 4 byte integers in shadow memory.  A separate
   alias set is used for all shadow memory accesses.  */
static GTY(()) tree shadow_ptr_types[3];

/* Decl for __asan_option_detect_stack_use_after_return.  */
static GTY(()) tree asan_detect_stack_use_after_return;

/* Hashtable support for memory references used by gimple
   statements.  */

/* This type represents a reference to a memory region.  */
struct asan_mem_ref
{
  /* The expression of the beginning of the memory region.  */
  tree start;

  /* The size of the access.  */
  HOST_WIDE_INT access_size;
};

object_allocator <asan_mem_ref> asan_mem_ref_pool ("asan_mem_ref");

/* Initializes an instance of asan_mem_ref.  */

static void
asan_mem_ref_init (asan_mem_ref *ref, tree start, HOST_WIDE_INT access_size)
{
  ref->start = start;
  ref->access_size = access_size;
}

/* Allocates memory for an instance of asan_mem_ref into the memory
   pool returned by asan_mem_ref_get_alloc_pool and initialize it.
   START is the address of (or the expression pointing to) the
   beginning of memory reference.  ACCESS_SIZE is the size of the
   access to the referenced memory.  */

static asan_mem_ref*
asan_mem_ref_new (tree start, HOST_WIDE_INT access_size)
{
  asan_mem_ref *ref = asan_mem_ref_pool.allocate ();

  asan_mem_ref_init (ref, start, access_size);
  return ref;
}

/* This builds and returns a pointer to the end of the memory region
   that starts at START and of length LEN.  */

tree
asan_mem_ref_get_end (tree start, tree len)
{
  if (len == NULL_TREE || integer_zerop (len))
    return start;

  if (!ptrofftype_p (len))
    len = convert_to_ptrofftype (len);

  return fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (start), start, len);
}

/*  Return a tree expression that represents the end of the referenced
    memory region.  Beware that this function can actually build a new
    tree expression.  */

tree
asan_mem_ref_get_end (const asan_mem_ref *ref, tree len)
{
  return asan_mem_ref_get_end (ref->start, len);
}

struct asan_mem_ref_hasher : nofree_ptr_hash <asan_mem_ref>
{
  static inline hashval_t hash (const asan_mem_ref *);
  static inline bool equal (const asan_mem_ref *, const asan_mem_ref *);
};

/* Hash a memory reference.  */

inline hashval_t
asan_mem_ref_hasher::hash (const asan_mem_ref *mem_ref)
{
  return iterative_hash_expr (mem_ref->start, 0);
}

/* Compare two memory references.  We accept the length of either
   memory references to be NULL_TREE.  */

inline bool
asan_mem_ref_hasher::equal (const asan_mem_ref *m1,
			    const asan_mem_ref *m2)
{
  return operand_equal_p (m1->start, m2->start, 0);
}

static hash_table<asan_mem_ref_hasher> *asan_mem_ref_ht;

/* Returns a reference to the hash table containing memory references.
   This function ensures that the hash table is created.  Note that
   this hash table is updated by the function
   update_mem_ref_hash_table.  */

static hash_table<asan_mem_ref_hasher> *
get_mem_ref_hash_table ()
{
  if (!asan_mem_ref_ht)
    asan_mem_ref_ht = new hash_table<asan_mem_ref_hasher> (10);

  return asan_mem_ref_ht;
}

/* Clear all entries from the memory references hash table.  */

static void
empty_mem_ref_hash_table ()
{
  if (asan_mem_ref_ht)
    asan_mem_ref_ht->empty ();
}

/* Free the memory references hash table.  */

static void
free_mem_ref_resources ()
{
  delete asan_mem_ref_ht;
  asan_mem_ref_ht = NULL;

  asan_mem_ref_pool.release ();
}

/* Return true iff the memory reference REF has been instrumented.  */

static bool
has_mem_ref_been_instrumented (tree ref, HOST_WIDE_INT access_size)
{
  asan_mem_ref r;
  asan_mem_ref_init (&r, ref, access_size);

  asan_mem_ref *saved_ref = get_mem_ref_hash_table ()->find (&r);
  return saved_ref && saved_ref->access_size >= access_size;
}

/* Return true iff the memory reference REF has been instrumented.  */

static bool
has_mem_ref_been_instrumented (const asan_mem_ref *ref)
{
  return has_mem_ref_been_instrumented (ref->start, ref->access_size);
}

/* Return true iff access to memory region starting at REF and of
   length LEN has been instrumented.  */

static bool
has_mem_ref_been_instrumented (const asan_mem_ref *ref, tree len)
{
  HOST_WIDE_INT size_in_bytes
    = tree_fits_shwi_p (len) ? tree_to_shwi (len) : -1;

  return size_in_bytes != -1
    && has_mem_ref_been_instrumented (ref->start, size_in_bytes);
}

/* Set REF to the memory reference present in a gimple assignment
   ASSIGNMENT.  Return true upon successful completion, false
   otherwise.  */

static bool
get_mem_ref_of_assignment (const gassign *assignment,
			   asan_mem_ref *ref,
			   bool *ref_is_store)
{
  gcc_assert (gimple_assign_single_p (assignment));

  if (gimple_store_p (assignment)
      && !gimple_clobber_p (assignment))
    {
      ref->start = gimple_assign_lhs (assignment);
      *ref_is_store = true;
    }
  else if (gimple_assign_load_p (assignment))
    {
      ref->start = gimple_assign_rhs1 (assignment);
      *ref_is_store = false;
    }
  else
    return false;

  ref->access_size = int_size_in_bytes (TREE_TYPE (ref->start));
  return true;
}

/* Return address of last allocated dynamic alloca.  */

static tree
get_last_alloca_addr ()
{
  if (last_alloca_addr)
    return last_alloca_addr;

  last_alloca_addr = create_tmp_reg (ptr_type_node, "last_alloca_addr");
  gassign *g = gimple_build_assign (last_alloca_addr, null_pointer_node);
  edge e = single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  gsi_insert_on_edge_immediate (e, g);
  return last_alloca_addr;
}

/* Insert __asan_allocas_unpoison (top, bottom) call after
   __builtin_stack_restore (new_sp) call.
   The pseudocode of this routine should look like this:
     __builtin_stack_restore (new_sp);
     top = last_alloca_addr;
     bot = new_sp;
     __asan_allocas_unpoison (top, bot);
     last_alloca_addr = new_sp;
   In general, we can't use new_sp as bot parameter because on some
   architectures SP has non zero offset from dynamic stack area.  Moreover, on
   some architectures this offset (STACK_DYNAMIC_OFFSET) becomes known for each
   particular function only after all callees were expanded to rtl.
   The most noticeable example is PowerPC{,64}, see
   http://refspecs.linuxfoundation.org/ELF/ppc64/PPC-elf64abi.html#DYNAM-STACK.
   To overcome the issue we use following trick: pass new_sp as a second
   parameter to __asan_allocas_unpoison and rewrite it during expansion with
   virtual_dynamic_stack_rtx later in expand_asan_emit_allocas_unpoison
   function.
*/

static void
handle_builtin_stack_restore (gcall *call, gimple_stmt_iterator *iter)
{
  if (!iter || !asan_sanitize_allocas_p ())
    return;

  tree last_alloca = get_last_alloca_addr ();
  tree restored_stack = gimple_call_arg (call, 0);
  tree fn = builtin_decl_implicit (BUILT_IN_ASAN_ALLOCAS_UNPOISON);
  gimple *g = gimple_build_call (fn, 2, last_alloca, restored_stack);
  gsi_insert_after (iter, g, GSI_NEW_STMT);
  g = gimple_build_assign (last_alloca, restored_stack);
  gsi_insert_after (iter, g, GSI_NEW_STMT);
}

/* Deploy and poison redzones around __builtin_alloca call.  To do this, we
   should replace this call with another one with changed parameters and
   replace all its uses with new address, so
       addr = __builtin_alloca (old_size, align);
   is replaced by
       left_redzone_size = max (align, ASAN_RED_ZONE_SIZE);
   Following two statements are optimized out if we know that
   old_size & (ASAN_RED_ZONE_SIZE - 1) == 0, i.e. alloca doesn't need partial
   redzone.
       misalign = old_size & (ASAN_RED_ZONE_SIZE - 1);
       partial_redzone_size = ASAN_RED_ZONE_SIZE - misalign;
       right_redzone_size = ASAN_RED_ZONE_SIZE;
       additional_size = left_redzone_size + partial_redzone_size +
                         right_redzone_size;
       new_size = old_size + additional_size;
       new_alloca = __builtin_alloca (new_size, max (align, 32))
       __asan_alloca_poison (new_alloca, old_size)
       addr = new_alloca + max (align, ASAN_RED_ZONE_SIZE);
       last_alloca_addr = new_alloca;
   ADDITIONAL_SIZE is added to make new memory allocation contain not only
   requested memory, but also left, partial and right redzones as well as some
   additional space, required by alignment.  */

static void
handle_builtin_alloca (gcall *call, gimple_stmt_iterator *iter)
{
  if (!iter || !asan_sanitize_allocas_p ())
    return;

  gassign *g;
  gcall *gg;
  const HOST_WIDE_INT redzone_mask = ASAN_RED_ZONE_SIZE - 1;

  tree last_alloca = get_last_alloca_addr ();
  tree callee = gimple_call_fndecl (call);
  tree old_size = gimple_call_arg (call, 0);
  tree ptr_type = gimple_call_lhs (call) ? TREE_TYPE (gimple_call_lhs (call))
					 : ptr_type_node;
  tree partial_size = NULL_TREE;
  bool alloca_with_align
    = DECL_FUNCTION_CODE (callee) == BUILT_IN_ALLOCA_WITH_ALIGN;
  unsigned int align
    = alloca_with_align ? tree_to_uhwi (gimple_call_arg (call, 1)) : 0;

  /* If ALIGN > ASAN_RED_ZONE_SIZE, we embed left redzone into first ALIGN
     bytes of allocated space.  Otherwise, align alloca to ASAN_RED_ZONE_SIZE
     manually.  */
  align = MAX (align, ASAN_RED_ZONE_SIZE * BITS_PER_UNIT);

  tree alloca_rz_mask = build_int_cst (size_type_node, redzone_mask);
  tree redzone_size = build_int_cst (size_type_node, ASAN_RED_ZONE_SIZE);

  /* Extract lower bits from old_size.  */
  wide_int size_nonzero_bits = get_nonzero_bits (old_size);
  wide_int rz_mask
    = wi::uhwi (redzone_mask, wi::get_precision (size_nonzero_bits));
  wide_int old_size_lower_bits = wi::bit_and (size_nonzero_bits, rz_mask);

  /* If alloca size is aligned to ASAN_RED_ZONE_SIZE, we don't need partial
     redzone.  Otherwise, compute its size here.  */
  if (wi::ne_p (old_size_lower_bits, 0))
    {
      /* misalign = size & (ASAN_RED_ZONE_SIZE - 1)
         partial_size = ASAN_RED_ZONE_SIZE - misalign.  */
      g = gimple_build_assign (make_ssa_name (size_type_node, NULL),
			       BIT_AND_EXPR, old_size, alloca_rz_mask);
      gsi_insert_before (iter, g, GSI_SAME_STMT);
      tree misalign = gimple_assign_lhs (g);
      g = gimple_build_assign (make_ssa_name (size_type_node, NULL), MINUS_EXPR,
			       redzone_size, misalign);
      gsi_insert_before (iter, g, GSI_SAME_STMT);
      partial_size = gimple_assign_lhs (g);
    }

  /* additional_size = align + ASAN_RED_ZONE_SIZE.  */
  tree additional_size = build_int_cst (size_type_node, align / BITS_PER_UNIT
							+ ASAN_RED_ZONE_SIZE);
  /* If alloca has partial redzone, include it to additional_size too.  */
  if (partial_size)
    {
      /* additional_size += partial_size.  */
      g = gimple_build_assign (make_ssa_name (size_type_node), PLUS_EXPR,
			       partial_size, additional_size);
      gsi_insert_before (iter, g, GSI_SAME_STMT);
      additional_size = gimple_assign_lhs (g);
    }

  /* new_size = old_size + additional_size.  */
  g = gimple_build_assign (make_ssa_name (size_type_node), PLUS_EXPR, old_size,
			   additional_size);
  gsi_insert_before (iter, g, GSI_SAME_STMT);
  tree new_size = gimple_assign_lhs (g);

  /* Build new __builtin_alloca call:
       new_alloca_with_rz = __builtin_alloca (new_size, align).  */
  tree fn = builtin_decl_implicit (BUILT_IN_ALLOCA_WITH_ALIGN);
  gg = gimple_build_call (fn, 2, new_size,
			  build_int_cst (size_type_node, align));
  tree new_alloca_with_rz = make_ssa_name (ptr_type, gg);
  gimple_call_set_lhs (gg, new_alloca_with_rz);
  gsi_insert_before (iter, gg, GSI_SAME_STMT);

  /* new_alloca = new_alloca_with_rz + align.  */
  g = gimple_build_assign (make_ssa_name (ptr_type), POINTER_PLUS_EXPR,
			   new_alloca_with_rz,
			   build_int_cst (size_type_node,
					  align / BITS_PER_UNIT));
  gsi_insert_before (iter, g, GSI_SAME_STMT);
  tree new_alloca = gimple_assign_lhs (g);

  /* Poison newly created alloca redzones:
      __asan_alloca_poison (new_alloca, old_size).  */
  fn = builtin_decl_implicit (BUILT_IN_ASAN_ALLOCA_POISON);
  gg = gimple_build_call (fn, 2, new_alloca, old_size);
  gsi_insert_before (iter, gg, GSI_SAME_STMT);

  /* Save new_alloca_with_rz value into last_alloca to use it during
     allocas unpoisoning.  */
  g = gimple_build_assign (last_alloca, new_alloca_with_rz);
  gsi_insert_before (iter, g, GSI_SAME_STMT);

  /* Finally, replace old alloca ptr with NEW_ALLOCA.  */
  replace_call_with_value (iter, new_alloca);
}

/* Return the memory references contained in a gimple statement
   representing a builtin call that has to do with memory access.  */

static bool
get_mem_refs_of_builtin_call (gcall *call,
			      asan_mem_ref *src0,
			      tree *src0_len,
			      bool *src0_is_store,
			      asan_mem_ref *src1,
			      tree *src1_len,
			      bool *src1_is_store,
			      asan_mem_ref *dst,
			      tree *dst_len,
			      bool *dst_is_store,
			      bool *dest_is_deref,
			      bool *intercepted_p,
			      gimple_stmt_iterator *iter = NULL)
{
  gcc_checking_assert (gimple_call_builtin_p (call, BUILT_IN_NORMAL));

  tree callee = gimple_call_fndecl (call);
  tree source0 = NULL_TREE, source1 = NULL_TREE,
    dest = NULL_TREE, len = NULL_TREE;
  bool is_store = true, got_reference_p = false;
  HOST_WIDE_INT access_size = 1;

  *intercepted_p = asan_intercepted_p ((DECL_FUNCTION_CODE (callee)));

  switch (DECL_FUNCTION_CODE (callee))
    {
      /* (s, s, n) style memops.  */
    case BUILT_IN_BCMP:
    case BUILT_IN_MEMCMP:
      source0 = gimple_call_arg (call, 0);
      source1 = gimple_call_arg (call, 1);
      len = gimple_call_arg (call, 2);
      break;

      /* (src, dest, n) style memops.  */
    case BUILT_IN_BCOPY:
      source0 = gimple_call_arg (call, 0);
      dest = gimple_call_arg (call, 1);
      len = gimple_call_arg (call, 2);
      break;

      /* (dest, src, n) style memops.  */
    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMMOVE:
    case BUILT_IN_MEMMOVE_CHK:
    case BUILT_IN_MEMPCPY:
    case BUILT_IN_MEMPCPY_CHK:
      dest = gimple_call_arg (call, 0);
      source0 = gimple_call_arg (call, 1);
      len = gimple_call_arg (call, 2);
      break;

      /* (dest, n) style memops.  */
    case BUILT_IN_BZERO:
      dest = gimple_call_arg (call, 0);
      len = gimple_call_arg (call, 1);
      break;

      /* (dest, x, n) style memops*/
    case BUILT_IN_MEMSET:
    case BUILT_IN_MEMSET_CHK:
      dest = gimple_call_arg (call, 0);
      len = gimple_call_arg (call, 2);
      break;

    case BUILT_IN_STRLEN:
      source0 = gimple_call_arg (call, 0);
      len = gimple_call_lhs (call);
      break;

    case BUILT_IN_STACK_RESTORE:
      handle_builtin_stack_restore (call, iter);
      break;

    case BUILT_IN_ALLOCA_WITH_ALIGN:
    case BUILT_IN_ALLOCA:
      handle_builtin_alloca (call, iter);
      break;
    /* And now the __atomic* and __sync builtins.
       These are handled differently from the classical memory memory
       access builtins above.  */

    case BUILT_IN_ATOMIC_LOAD_1:
      is_store = false;
      /* FALLTHRU */
    case BUILT_IN_SYNC_FETCH_AND_ADD_1:
    case BUILT_IN_SYNC_FETCH_AND_SUB_1:
    case BUILT_IN_SYNC_FETCH_AND_OR_1:
    case BUILT_IN_SYNC_FETCH_AND_AND_1:
    case BUILT_IN_SYNC_FETCH_AND_XOR_1:
    case BUILT_IN_SYNC_FETCH_AND_NAND_1:
    case BUILT_IN_SYNC_ADD_AND_FETCH_1:
    case BUILT_IN_SYNC_SUB_AND_FETCH_1:
    case BUILT_IN_SYNC_OR_AND_FETCH_1:
    case BUILT_IN_SYNC_AND_AND_FETCH_1:
    case BUILT_IN_SYNC_XOR_AND_FETCH_1:
    case BUILT_IN_SYNC_NAND_AND_FETCH_1:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_1:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_1:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_1:
    case BUILT_IN_SYNC_LOCK_RELEASE_1:
    case BUILT_IN_ATOMIC_EXCHANGE_1:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_1:
    case BUILT_IN_ATOMIC_STORE_1:
    case BUILT_IN_ATOMIC_ADD_FETCH_1:
    case BUILT_IN_ATOMIC_SUB_FETCH_1:
    case BUILT_IN_ATOMIC_AND_FETCH_1:
    case BUILT_IN_ATOMIC_NAND_FETCH_1:
    case BUILT_IN_ATOMIC_XOR_FETCH_1:
    case BUILT_IN_ATOMIC_OR_FETCH_1:
    case BUILT_IN_ATOMIC_FETCH_ADD_1:
    case BUILT_IN_ATOMIC_FETCH_SUB_1:
    case BUILT_IN_ATOMIC_FETCH_AND_1:
    case BUILT_IN_ATOMIC_FETCH_NAND_1:
    case BUILT_IN_ATOMIC_FETCH_XOR_1:
    case BUILT_IN_ATOMIC_FETCH_OR_1:
      access_size = 1;
      goto do_atomic;

    case BUILT_IN_ATOMIC_LOAD_2:
      is_store = false;
      /* FALLTHRU */
    case BUILT_IN_SYNC_FETCH_AND_ADD_2:
    case BUILT_IN_SYNC_FETCH_AND_SUB_2:
    case BUILT_IN_SYNC_FETCH_AND_OR_2:
    case BUILT_IN_SYNC_FETCH_AND_AND_2:
    case BUILT_IN_SYNC_FETCH_AND_XOR_2:
    case BUILT_IN_SYNC_FETCH_AND_NAND_2:
    case BUILT_IN_SYNC_ADD_AND_FETCH_2:
    case BUILT_IN_SYNC_SUB_AND_FETCH_2:
    case BUILT_IN_SYNC_OR_AND_FETCH_2:
    case BUILT_IN_SYNC_AND_AND_FETCH_2:
    case BUILT_IN_SYNC_XOR_AND_FETCH_2:
    case BUILT_IN_SYNC_NAND_AND_FETCH_2:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_2:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_2:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_2:
    case BUILT_IN_SYNC_LOCK_RELEASE_2:
    case BUILT_IN_ATOMIC_EXCHANGE_2:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_2:
    case BUILT_IN_ATOMIC_STORE_2:
    case BUILT_IN_ATOMIC_ADD_FETCH_2:
    case BUILT_IN_ATOMIC_SUB_FETCH_2:
    case BUILT_IN_ATOMIC_AND_FETCH_2:
    case BUILT_IN_ATOMIC_NAND_FETCH_2:
    case BUILT_IN_ATOMIC_XOR_FETCH_2:
    case BUILT_IN_ATOMIC_OR_FETCH_2:
    case BUILT_IN_ATOMIC_FETCH_ADD_2:
    case BUILT_IN_ATOMIC_FETCH_SUB_2:
    case BUILT_IN_ATOMIC_FETCH_AND_2:
    case BUILT_IN_ATOMIC_FETCH_NAND_2:
    case BUILT_IN_ATOMIC_FETCH_XOR_2:
    case BUILT_IN_ATOMIC_FETCH_OR_2:
      access_size = 2;
      goto do_atomic;

    case BUILT_IN_ATOMIC_LOAD_4:
      is_store = false;
      /* FALLTHRU */
    case BUILT_IN_SYNC_FETCH_AND_ADD_4:
    case BUILT_IN_SYNC_FETCH_AND_SUB_4:
    case BUILT_IN_SYNC_FETCH_AND_OR_4:
    case BUILT_IN_SYNC_FETCH_AND_AND_4:
    case BUILT_IN_SYNC_FETCH_AND_XOR_4:
    case BUILT_IN_SYNC_FETCH_AND_NAND_4:
    case BUILT_IN_SYNC_ADD_AND_FETCH_4:
    case BUILT_IN_SYNC_SUB_AND_FETCH_4:
    case BUILT_IN_SYNC_OR_AND_FETCH_4:
    case BUILT_IN_SYNC_AND_AND_FETCH_4:
    case BUILT_IN_SYNC_XOR_AND_FETCH_4:
    case BUILT_IN_SYNC_NAND_AND_FETCH_4:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_4:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_4:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_4:
    case BUILT_IN_SYNC_LOCK_RELEASE_4:
    case BUILT_IN_ATOMIC_EXCHANGE_4:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_4:
    case BUILT_IN_ATOMIC_STORE_4:
    case BUILT_IN_ATOMIC_ADD_FETCH_4:
    case BUILT_IN_ATOMIC_SUB_FETCH_4:
    case BUILT_IN_ATOMIC_AND_FETCH_4:
    case BUILT_IN_ATOMIC_NAND_FETCH_4:
    case BUILT_IN_ATOMIC_XOR_FETCH_4:
    case BUILT_IN_ATOMIC_OR_FETCH_4:
    case BUILT_IN_ATOMIC_FETCH_ADD_4:
    case BUILT_IN_ATOMIC_FETCH_SUB_4:
    case BUILT_IN_ATOMIC_FETCH_AND_4:
    case BUILT_IN_ATOMIC_FETCH_NAND_4:
    case BUILT_IN_ATOMIC_FETCH_XOR_4:
    case BUILT_IN_ATOMIC_FETCH_OR_4:
      access_size = 4;
      goto do_atomic;

    case BUILT_IN_ATOMIC_LOAD_8:
      is_store = false;
      /* FALLTHRU */
    case BUILT_IN_SYNC_FETCH_AND_ADD_8:
    case BUILT_IN_SYNC_FETCH_AND_SUB_8:
    case BUILT_IN_SYNC_FETCH_AND_OR_8:
    case BUILT_IN_SYNC_FETCH_AND_AND_8:
    case BUILT_IN_SYNC_FETCH_AND_XOR_8:
    case BUILT_IN_SYNC_FETCH_AND_NAND_8:
    case BUILT_IN_SYNC_ADD_AND_FETCH_8:
    case BUILT_IN_SYNC_SUB_AND_FETCH_8:
    case BUILT_IN_SYNC_OR_AND_FETCH_8:
    case BUILT_IN_SYNC_AND_AND_FETCH_8:
    case BUILT_IN_SYNC_XOR_AND_FETCH_8:
    case BUILT_IN_SYNC_NAND_AND_FETCH_8:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_8:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_8:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_8:
    case BUILT_IN_SYNC_LOCK_RELEASE_8:
    case BUILT_IN_ATOMIC_EXCHANGE_8:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_8:
    case BUILT_IN_ATOMIC_STORE_8:
    case BUILT_IN_ATOMIC_ADD_FETCH_8:
    case BUILT_IN_ATOMIC_SUB_FETCH_8:
    case BUILT_IN_ATOMIC_AND_FETCH_8:
    case BUILT_IN_ATOMIC_NAND_FETCH_8:
    case BUILT_IN_ATOMIC_XOR_FETCH_8:
    case BUILT_IN_ATOMIC_OR_FETCH_8:
    case BUILT_IN_ATOMIC_FETCH_ADD_8:
    case BUILT_IN_ATOMIC_FETCH_SUB_8:
    case BUILT_IN_ATOMIC_FETCH_AND_8:
    case BUILT_IN_ATOMIC_FETCH_NAND_8:
    case BUILT_IN_ATOMIC_FETCH_XOR_8:
    case BUILT_IN_ATOMIC_FETCH_OR_8:
      access_size = 8;
      goto do_atomic;

    case BUILT_IN_ATOMIC_LOAD_16:
      is_store = false;
      /* FALLTHRU */
    case BUILT_IN_SYNC_FETCH_AND_ADD_16:
    case BUILT_IN_SYNC_FETCH_AND_SUB_16:
    case BUILT_IN_SYNC_FETCH_AND_OR_16:
    case BUILT_IN_SYNC_FETCH_AND_AND_16:
    case BUILT_IN_SYNC_FETCH_AND_XOR_16:
    case BUILT_IN_SYNC_FETCH_AND_NAND_16:
    case BUILT_IN_SYNC_ADD_AND_FETCH_16:
    case BUILT_IN_SYNC_SUB_AND_FETCH_16:
    case BUILT_IN_SYNC_OR_AND_FETCH_16:
    case BUILT_IN_SYNC_AND_AND_FETCH_16:
    case BUILT_IN_SYNC_XOR_AND_FETCH_16:
    case BUILT_IN_SYNC_NAND_AND_FETCH_16:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_16:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_16:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_16:
    case BUILT_IN_SYNC_LOCK_RELEASE_16:
    case BUILT_IN_ATOMIC_EXCHANGE_16:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_16:
    case BUILT_IN_ATOMIC_STORE_16:
    case BUILT_IN_ATOMIC_ADD_FETCH_16:
    case BUILT_IN_ATOMIC_SUB_FETCH_16:
    case BUILT_IN_ATOMIC_AND_FETCH_16:
    case BUILT_IN_ATOMIC_NAND_FETCH_16:
    case BUILT_IN_ATOMIC_XOR_FETCH_16:
    case BUILT_IN_ATOMIC_OR_FETCH_16:
    case BUILT_IN_ATOMIC_FETCH_ADD_16:
    case BUILT_IN_ATOMIC_FETCH_SUB_16:
    case BUILT_IN_ATOMIC_FETCH_AND_16:
    case BUILT_IN_ATOMIC_FETCH_NAND_16:
    case BUILT_IN_ATOMIC_FETCH_XOR_16:
    case BUILT_IN_ATOMIC_FETCH_OR_16:
      access_size = 16;
      /* FALLTHRU */
    do_atomic:
      {
	dest = gimple_call_arg (call, 0);
	/* DEST represents the address of a memory location.
	   instrument_derefs wants the memory location, so lets
	   dereference the address DEST before handing it to
	   instrument_derefs.  */
	tree type = build_nonstandard_integer_type (access_size
						    * BITS_PER_UNIT, 1);
	dest = build2 (MEM_REF, type, dest,
		       build_int_cst (build_pointer_type (char_type_node), 0));
	break;
      }

    default:
      /* The other builtins memory access are not instrumented in this
	 function because they either don't have any length parameter,
	 or their length parameter is just a limit.  */
      break;
    }

  if (len != NULL_TREE)
    {
      if (source0 != NULL_TREE)
	{
	  src0->start = source0;
	  src0->access_size = access_size;
	  *src0_len = len;
	  *src0_is_store = false;
	}

      if (source1 != NULL_TREE)
	{
	  src1->start = source1;
	  src1->access_size = access_size;
	  *src1_len = len;
	  *src1_is_store = false;
	}

      if (dest != NULL_TREE)
	{
	  dst->start = dest;
	  dst->access_size = access_size;
	  *dst_len = len;
	  *dst_is_store = true;
	}

      got_reference_p = true;
    }
  else if (dest)
    {
      dst->start = dest;
      dst->access_size = access_size;
      *dst_len = NULL_TREE;
      *dst_is_store = is_store;
      *dest_is_deref = true;
      got_reference_p = true;
    }

  return got_reference_p;
}

/* Return true iff a given gimple statement has been instrumented.
   Note that the statement is "defined" by the memory references it
   contains.  */

static bool
has_stmt_been_instrumented_p (gimple *stmt)
{
  if (gimple_assign_single_p (stmt))
    {
      bool r_is_store;
      asan_mem_ref r;
      asan_mem_ref_init (&r, NULL, 1);

      if (get_mem_ref_of_assignment (as_a <gassign *> (stmt), &r,
				     &r_is_store))
	return has_mem_ref_been_instrumented (&r);
    }
  else if (gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
    {
      asan_mem_ref src0, src1, dest;
      asan_mem_ref_init (&src0, NULL, 1);
      asan_mem_ref_init (&src1, NULL, 1);
      asan_mem_ref_init (&dest, NULL, 1);

      tree src0_len = NULL_TREE, src1_len = NULL_TREE, dest_len = NULL_TREE;
      bool src0_is_store = false, src1_is_store = false,
	dest_is_store = false, dest_is_deref = false, intercepted_p = true;
      if (get_mem_refs_of_builtin_call (as_a <gcall *> (stmt),
					&src0, &src0_len, &src0_is_store,
					&src1, &src1_len, &src1_is_store,
					&dest, &dest_len, &dest_is_store,
					&dest_is_deref, &intercepted_p))
	{
	  if (src0.start != NULL_TREE
	      && !has_mem_ref_been_instrumented (&src0, src0_len))
	    return false;

	  if (src1.start != NULL_TREE
	      && !has_mem_ref_been_instrumented (&src1, src1_len))
	    return false;

	  if (dest.start != NULL_TREE
	      && !has_mem_ref_been_instrumented (&dest, dest_len))
	    return false;

	  return true;
	}
    }
  else if (is_gimple_call (stmt) && gimple_store_p (stmt))
    {
      asan_mem_ref r;
      asan_mem_ref_init (&r, NULL, 1);

      r.start = gimple_call_lhs (stmt);
      r.access_size = int_size_in_bytes (TREE_TYPE (r.start));
      return has_mem_ref_been_instrumented (&r);
    }

  return false;
}

/*  Insert a memory reference into the hash table.  */

static void
update_mem_ref_hash_table (tree ref, HOST_WIDE_INT access_size)
{
  hash_table<asan_mem_ref_hasher> *ht = get_mem_ref_hash_table ();

  asan_mem_ref r;
  asan_mem_ref_init (&r, ref, access_size);

  asan_mem_ref **slot = ht->find_slot (&r, INSERT);
  if (*slot == NULL || (*slot)->access_size < access_size)
    *slot = asan_mem_ref_new (ref, access_size);
}

/* Initialize shadow_ptr_types array.  */

static void
asan_init_shadow_ptr_types (void)
{
  asan_shadow_set = new_alias_set ();
  tree types[3] = { signed_char_type_node, short_integer_type_node,
		    integer_type_node };

  for (unsigned i = 0; i < 3; i++)
    {
      shadow_ptr_types[i] = build_distinct_type_copy (types[i]);
      TYPE_ALIAS_SET (shadow_ptr_types[i]) = asan_shadow_set;
      shadow_ptr_types[i] = build_pointer_type (shadow_ptr_types[i]);
    }

  initialize_sanitizer_builtins ();
}

/* Create ADDR_EXPR of STRING_CST with the PP pretty printer text.  */

static tree
asan_pp_string (pretty_printer *pp)
{
  const char *buf = pp_formatted_text (pp);
  size_t len = strlen (buf);
  tree ret = build_string (len + 1, buf);
  TREE_TYPE (ret)
    = build_array_type (TREE_TYPE (shadow_ptr_types[0]),
			build_index_type (size_int (len)));
  TREE_READONLY (ret) = 1;
  TREE_STATIC (ret) = 1;
  return build1 (ADDR_EXPR, shadow_ptr_types[0], ret);
}

/* Return a CONST_INT representing 4 subsequent shadow memory bytes.  */

static rtx
asan_shadow_cst (unsigned char shadow_bytes[4])
{
  int i;
  unsigned HOST_WIDE_INT val = 0;
  gcc_assert (WORDS_BIG_ENDIAN == BYTES_BIG_ENDIAN);
  for (i = 0; i < 4; i++)
    val |= (unsigned HOST_WIDE_INT) shadow_bytes[BYTES_BIG_ENDIAN ? 3 - i : i]
	   << (BITS_PER_UNIT * i);
  return gen_int_mode (val, SImode);
}

/* Clear shadow memory at SHADOW_MEM, LEN bytes.  Can't call a library call here
   though.  */

static void
asan_clear_shadow (rtx shadow_mem, HOST_WIDE_INT len)
{
  rtx_insn *insn, *insns, *jump;
  rtx_code_label *top_label;
  rtx end, addr, tmp;

  start_sequence ();
  clear_storage (shadow_mem, GEN_INT (len), BLOCK_OP_NORMAL);
  insns = get_insns ();
  end_sequence ();
  for (insn = insns; insn; insn = NEXT_INSN (insn))
    if (CALL_P (insn))
      break;
  if (insn == NULL_RTX)
    {
      emit_insn (insns);
      return;
    }

  gcc_assert ((len & 3) == 0);
  top_label = gen_label_rtx ();
  addr = copy_to_mode_reg (Pmode, XEXP (shadow_mem, 0));
  shadow_mem = adjust_automodify_address (shadow_mem, SImode, addr, 0);
  end = force_reg (Pmode, plus_constant (Pmode, addr, len));
  emit_label (top_label);

  emit_move_insn (shadow_mem, const0_rtx);
  tmp = expand_simple_binop (Pmode, PLUS, addr, gen_int_mode (4, Pmode), addr,
			     true, OPTAB_LIB_WIDEN);
  if (tmp != addr)
    emit_move_insn (addr, tmp);
  emit_cmp_and_jump_insns (addr, end, LT, NULL_RTX, Pmode, true, top_label);
  jump = get_last_insn ();
  gcc_assert (JUMP_P (jump));
  add_reg_br_prob_note (jump,
			profile_probability::guessed_always ()
			   .apply_scale (80, 100));
}

void
asan_function_start (void)
{
  section *fnsec = function_section (current_function_decl);
  switch_to_section (fnsec);
  ASM_OUTPUT_DEBUG_LABEL (asm_out_file, "LASANPC",
			 current_function_funcdef_no);
}

/* Return number of shadow bytes that are occupied by a local variable
   of SIZE bytes.  */

static unsigned HOST_WIDE_INT
shadow_mem_size (unsigned HOST_WIDE_INT size)
{
  return ROUND_UP (size, ASAN_SHADOW_GRANULARITY) / ASAN_SHADOW_GRANULARITY;
}

/* Insert code to protect stack vars.  The prologue sequence should be emitted
   directly, epilogue sequence returned.  BASE is the register holding the
   stack base, against which OFFSETS array offsets are relative to, OFFSETS
   array contains pairs of offsets in reverse order, always the end offset
   of some gap that needs protection followed by starting offset,
   and DECLS is an array of representative decls for each var partition.
   LENGTH is the length of the OFFSETS array, DECLS array is LENGTH / 2 - 1
   elements long (OFFSETS include gap before the first variable as well
   as gaps after each stack variable).  PBASE is, if non-NULL, some pseudo
   register which stack vars DECL_RTLs are based on.  Either BASE should be
   assigned to PBASE, when not doing use after return protection, or
   corresponding address based on __asan_stack_malloc* return value.  */

rtx_insn *
asan_emit_stack_protection (rtx base, rtx pbase, unsigned int alignb,
			    HOST_WIDE_INT *offsets, tree *decls, int length)
{
  rtx shadow_base, shadow_mem, ret, mem, orig_base;
  rtx_code_label *lab;
  rtx_insn *insns;
  char buf[32];
  unsigned char shadow_bytes[4];
  HOST_WIDE_INT base_offset = offsets[length - 1];
  HOST_WIDE_INT base_align_bias = 0, offset, prev_offset;
  HOST_WIDE_INT asan_frame_size = offsets[0] - base_offset;
  HOST_WIDE_INT last_offset, last_size;
  int l;
  unsigned char cur_shadow_byte = ASAN_STACK_MAGIC_LEFT;
  tree str_cst, decl, id;
  int use_after_return_class = -1;

  if (shadow_ptr_types[0] == NULL_TREE)
    asan_init_shadow_ptr_types ();

  /* First of all, prepare the description string.  */
  pretty_printer asan_pp;

  pp_decimal_int (&asan_pp, length / 2 - 1);
  pp_space (&asan_pp);
  for (l = length - 2; l; l -= 2)
    {
      tree decl = decls[l / 2 - 1];
      pp_wide_integer (&asan_pp, offsets[l] - base_offset);
      pp_space (&asan_pp);
      pp_wide_integer (&asan_pp, offsets[l - 1] - offsets[l]);
      pp_space (&asan_pp);
      if (DECL_P (decl) && DECL_NAME (decl))
	{
	  pp_decimal_int (&asan_pp, IDENTIFIER_LENGTH (DECL_NAME (decl)));
	  pp_space (&asan_pp);
	  pp_tree_identifier (&asan_pp, DECL_NAME (decl));
	}
      else
	pp_string (&asan_pp, "9 <unknown>");
      pp_space (&asan_pp);
    }
  str_cst = asan_pp_string (&asan_pp);

  /* Emit the prologue sequence.  */
  if (asan_frame_size > 32 && asan_frame_size <= 65536 && pbase
      && ASAN_USE_AFTER_RETURN)
    {
      use_after_return_class = floor_log2 (asan_frame_size - 1) - 5;
      /* __asan_stack_malloc_N guarantees alignment
	 N < 6 ? (64 << N) : 4096 bytes.  */
      if (alignb > (use_after_return_class < 6
		    ? (64U << use_after_return_class) : 4096U))
	use_after_return_class = -1;
      else if (alignb > ASAN_RED_ZONE_SIZE && (asan_frame_size & (alignb - 1)))
	base_align_bias = ((asan_frame_size + alignb - 1)
			   & ~(alignb - HOST_WIDE_INT_1)) - asan_frame_size;
    }
  /* Align base if target is STRICT_ALIGNMENT.  */
  if (STRICT_ALIGNMENT)
    base = expand_binop (Pmode, and_optab, base,
			 gen_int_mode (-((GET_MODE_ALIGNMENT (SImode)
					  << ASAN_SHADOW_SHIFT)
					 / BITS_PER_UNIT), Pmode), NULL_RTX,
			 1, OPTAB_DIRECT);

  if (use_after_return_class == -1 && pbase)
    emit_move_insn (pbase, base);

  base = expand_binop (Pmode, add_optab, base,
		       gen_int_mode (base_offset - base_align_bias, Pmode),
		       NULL_RTX, 1, OPTAB_DIRECT);
  orig_base = NULL_RTX;
  if (use_after_return_class != -1)
    {
      if (asan_detect_stack_use_after_return == NULL_TREE)
	{
	  id = get_identifier ("__asan_option_detect_stack_use_after_return");
	  decl = build_decl (BUILTINS_LOCATION, VAR_DECL, id,
			     integer_type_node);
	  SET_DECL_ASSEMBLER_NAME (decl, id);
	  TREE_ADDRESSABLE (decl) = 1;
	  DECL_ARTIFICIAL (decl) = 1;
	  DECL_IGNORED_P (decl) = 1;
	  DECL_EXTERNAL (decl) = 1;
	  TREE_STATIC (decl) = 1;
	  TREE_PUBLIC (decl) = 1;
	  TREE_USED (decl) = 1;
	  asan_detect_stack_use_after_return = decl;
	}
      orig_base = gen_reg_rtx (Pmode);
      emit_move_insn (orig_base, base);
      ret = expand_normal (asan_detect_stack_use_after_return);
      lab = gen_label_rtx ();
      emit_cmp_and_jump_insns (ret, const0_rtx, EQ, NULL_RTX,
			       VOIDmode, 0, lab,
			       profile_probability::very_likely ());
      snprintf (buf, sizeof buf, "__asan_stack_malloc_%d",
		use_after_return_class);
      ret = init_one_libfunc (buf);
      ret = emit_library_call_value (ret, NULL_RTX, LCT_NORMAL, ptr_mode,
				     GEN_INT (asan_frame_size
					      + base_align_bias),
				     TYPE_MODE (pointer_sized_int_node));
      /* __asan_stack_malloc_[n] returns a pointer to fake stack if succeeded
	 and NULL otherwise.  Check RET value is NULL here and jump over the
	 BASE reassignment in this case.  Otherwise, reassign BASE to RET.  */
      emit_cmp_and_jump_insns (ret, const0_rtx, EQ, NULL_RTX,
			       VOIDmode, 0, lab,
			       profile_probability:: very_unlikely ());
      ret = convert_memory_address (Pmode, ret);
      emit_move_insn (base, ret);
      emit_label (lab);
      emit_move_insn (pbase, expand_binop (Pmode, add_optab, base,
					   gen_int_mode (base_align_bias
							 - base_offset, Pmode),
					   NULL_RTX, 1, OPTAB_DIRECT));
    }
  mem = gen_rtx_MEM (ptr_mode, base);
  mem = adjust_address (mem, VOIDmode, base_align_bias);
  emit_move_insn (mem, gen_int_mode (ASAN_STACK_FRAME_MAGIC, ptr_mode));
  mem = adjust_address (mem, VOIDmode, GET_MODE_SIZE (ptr_mode));
  emit_move_insn (mem, expand_normal (str_cst));
  mem = adjust_address (mem, VOIDmode, GET_MODE_SIZE (ptr_mode));
  ASM_GENERATE_INTERNAL_LABEL (buf, "LASANPC", current_function_funcdef_no);
  id = get_identifier (buf);
  decl = build_decl (DECL_SOURCE_LOCATION (current_function_decl),
		    VAR_DECL, id, char_type_node);
  SET_DECL_ASSEMBLER_NAME (decl, id);
  TREE_ADDRESSABLE (decl) = 1;
  TREE_READONLY (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  TREE_STATIC (decl) = 1;
  TREE_PUBLIC (decl) = 0;
  TREE_USED (decl) = 1;
  DECL_INITIAL (decl) = decl;
  TREE_ASM_WRITTEN (decl) = 1;
  TREE_ASM_WRITTEN (id) = 1;
  emit_move_insn (mem, expand_normal (build_fold_addr_expr (decl)));
  shadow_base = expand_binop (Pmode, lshr_optab, base,
			      GEN_INT (ASAN_SHADOW_SHIFT),
			      NULL_RTX, 1, OPTAB_DIRECT);
  shadow_base
    = plus_constant (Pmode, shadow_base,
		     asan_shadow_offset ()
		     + (base_align_bias >> ASAN_SHADOW_SHIFT));
  gcc_assert (asan_shadow_set != -1
	      && (ASAN_RED_ZONE_SIZE >> ASAN_SHADOW_SHIFT) == 4);
  shadow_mem = gen_rtx_MEM (SImode, shadow_base);
  set_mem_alias_set (shadow_mem, asan_shadow_set);
  if (STRICT_ALIGNMENT)
    set_mem_align (shadow_mem, (GET_MODE_ALIGNMENT (SImode)));
  prev_offset = base_offset;
  for (l = length; l; l -= 2)
    {
      if (l == 2)
	cur_shadow_byte = ASAN_STACK_MAGIC_RIGHT;
      offset = offsets[l - 1];
      if ((offset - base_offset) & (ASAN_RED_ZONE_SIZE - 1))
	{
	  int i;
	  HOST_WIDE_INT aoff
	    = base_offset + ((offset - base_offset)
			     & ~(ASAN_RED_ZONE_SIZE - HOST_WIDE_INT_1));
	  shadow_mem = adjust_address (shadow_mem, VOIDmode,
				       (aoff - prev_offset)
				       >> ASAN_SHADOW_SHIFT);
	  prev_offset = aoff;
	  for (i = 0; i < 4; i++, aoff += ASAN_SHADOW_GRANULARITY)
	    if (aoff < offset)
	      {
		if (aoff < offset - (HOST_WIDE_INT)ASAN_SHADOW_GRANULARITY + 1)
		  shadow_bytes[i] = 0;
		else
		  shadow_bytes[i] = offset - aoff;
	      }
	    else
	      shadow_bytes[i] = ASAN_STACK_MAGIC_MIDDLE;
	  emit_move_insn (shadow_mem, asan_shadow_cst (shadow_bytes));
	  offset = aoff;
	}
      while (offset <= offsets[l - 2] - ASAN_RED_ZONE_SIZE)
	{
	  shadow_mem = adjust_address (shadow_mem, VOIDmode,
				       (offset - prev_offset)
				       >> ASAN_SHADOW_SHIFT);
	  prev_offset = offset;
	  memset (shadow_bytes, cur_shadow_byte, 4);
	  emit_move_insn (shadow_mem, asan_shadow_cst (shadow_bytes));
	  offset += ASAN_RED_ZONE_SIZE;
	}
      cur_shadow_byte = ASAN_STACK_MAGIC_MIDDLE;
    }
  do_pending_stack_adjust ();

  /* Construct epilogue sequence.  */
  start_sequence ();

  lab = NULL;
  if (use_after_return_class != -1)
    {
      rtx_code_label *lab2 = gen_label_rtx ();
      char c = (char) ASAN_STACK_MAGIC_USE_AFTER_RET;
      emit_cmp_and_jump_insns (orig_base, base, EQ, NULL_RTX,
			       VOIDmode, 0, lab2,
			       profile_probability::very_likely ());
      shadow_mem = gen_rtx_MEM (BLKmode, shadow_base);
      set_mem_alias_set (shadow_mem, asan_shadow_set);
      mem = gen_rtx_MEM (ptr_mode, base);
      mem = adjust_address (mem, VOIDmode, base_align_bias);
      emit_move_insn (mem, gen_int_mode (ASAN_STACK_RETIRED_MAGIC, ptr_mode));
      unsigned HOST_WIDE_INT sz = asan_frame_size >> ASAN_SHADOW_SHIFT;
      if (use_after_return_class < 5
	  && can_store_by_pieces (sz, builtin_memset_read_str, &c,
				  BITS_PER_UNIT, true))
	store_by_pieces (shadow_mem, sz, builtin_memset_read_str, &c,
			 BITS_PER_UNIT, true, 0);
      else if (use_after_return_class >= 5
	       || !set_storage_via_setmem (shadow_mem,
					   GEN_INT (sz),
					   gen_int_mode (c, QImode),
					   BITS_PER_UNIT, BITS_PER_UNIT,
					   -1, sz, sz, sz))
	{
	  snprintf (buf, sizeof buf, "__asan_stack_free_%d",
		    use_after_return_class);
	  ret = init_one_libfunc (buf);
	  rtx addr = convert_memory_address (ptr_mode, base);
	  rtx orig_addr = convert_memory_address (ptr_mode, orig_base);
	  emit_library_call (ret, LCT_NORMAL, ptr_mode, addr, ptr_mode,
			     GEN_INT (asan_frame_size + base_align_bias),
			     TYPE_MODE (pointer_sized_int_node),
			     orig_addr, ptr_mode);
	}
      lab = gen_label_rtx ();
      emit_jump (lab);
      emit_label (lab2);
    }

  shadow_mem = gen_rtx_MEM (BLKmode, shadow_base);
  set_mem_alias_set (shadow_mem, asan_shadow_set);

  if (STRICT_ALIGNMENT)
    set_mem_align (shadow_mem, (GET_MODE_ALIGNMENT (SImode)));

  prev_offset = base_offset;
  last_offset = base_offset;
  last_size = 0;
  for (l = length; l; l -= 2)
    {
      offset = base_offset + ((offsets[l - 1] - base_offset)
			     & ~(ASAN_RED_ZONE_SIZE - HOST_WIDE_INT_1));
      if (last_offset + last_size != offset)
	{
	  shadow_mem = adjust_address (shadow_mem, VOIDmode,
				       (last_offset - prev_offset)
				       >> ASAN_SHADOW_SHIFT);
	  prev_offset = last_offset;
	  asan_clear_shadow (shadow_mem, last_size >> ASAN_SHADOW_SHIFT);
	  last_offset = offset;
	  last_size = 0;
	}
      last_size += base_offset + ((offsets[l - 2] - base_offset)
				  & ~(ASAN_RED_ZONE_SIZE - HOST_WIDE_INT_1))
		   - offset;

      /* Unpoison shadow memory that corresponds to a variable that is 
	 is subject of use-after-return sanitization.  */
      if (l > 2)
	{
	  decl = decls[l / 2 - 2];
	  if (asan_handled_variables != NULL
	      && asan_handled_variables->contains (decl))
	    {
	      HOST_WIDE_INT size = offsets[l - 3] - offsets[l - 2];
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  const char *n = (DECL_NAME (decl)
				   ? IDENTIFIER_POINTER (DECL_NAME (decl))
				   : "<unknown>");
		  fprintf (dump_file, "Unpoisoning shadow stack for variable: "
			   "%s (%" PRId64 " B)\n", n, size);
		}

		last_size += size & ~(ASAN_RED_ZONE_SIZE - HOST_WIDE_INT_1);
	    }
	}
    }
  if (last_size)
    {
      shadow_mem = adjust_address (shadow_mem, VOIDmode,
				   (last_offset - prev_offset)
				   >> ASAN_SHADOW_SHIFT);
      asan_clear_shadow (shadow_mem, last_size >> ASAN_SHADOW_SHIFT);
    }

  /* Clean-up set with instrumented stack variables.  */
  delete asan_handled_variables;
  asan_handled_variables = NULL;
  delete asan_used_labels;
  asan_used_labels = NULL;

  do_pending_stack_adjust ();
  if (lab)
    emit_label (lab);

  insns = get_insns ();
  end_sequence ();
  return insns;
}

/* Emit __asan_allocas_unpoison (top, bot) call.  The BASE parameter corresponds
   to BOT argument, for TOP virtual_stack_dynamic_rtx is used.  NEW_SEQUENCE
   indicates whether we're emitting new instructions sequence or not.  */

rtx_insn *
asan_emit_allocas_unpoison (rtx top, rtx bot, rtx_insn *before)
{
  if (before)
    push_to_sequence (before);
  else
    start_sequence ();
  rtx ret = init_one_libfunc ("__asan_allocas_unpoison");
  top = convert_memory_address (ptr_mode, top);
  bot = convert_memory_address (ptr_mode, bot);
  ret = emit_library_call_value (ret, NULL_RTX, LCT_NORMAL, ptr_mode,
				 top, ptr_mode, bot, ptr_mode);

  do_pending_stack_adjust ();
  rtx_insn *insns = get_insns ();
  end_sequence ();
  return insns;
}

/* Return true if DECL, a global var, might be overridden and needs
   therefore a local alias.  */

static bool
asan_needs_local_alias (tree decl)
{
  return DECL_WEAK (decl) || !targetm.binds_local_p (decl);
}

/* Return true if DECL, a global var, is an artificial ODR indicator symbol
   therefore doesn't need protection.  */

static bool
is_odr_indicator (tree decl)
{
  return (DECL_ARTIFICIAL (decl)
	  && lookup_attribute ("asan odr indicator", DECL_ATTRIBUTES (decl)));
}

/* Return true if DECL is a VAR_DECL that should be protected
   by Address Sanitizer, by appending a red zone with protected
   shadow memory after it and aligning it to at least
   ASAN_RED_ZONE_SIZE bytes.  */

bool
asan_protect_global (tree decl)
{
  if (!ASAN_GLOBALS)
    return false;

  rtx rtl, symbol;

  if (TREE_CODE (decl) == STRING_CST)
    {
      /* Instrument all STRING_CSTs except those created
	 by asan_pp_string here.  */
      if (shadow_ptr_types[0] != NULL_TREE
	  && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE
	  && TREE_TYPE (TREE_TYPE (decl)) == TREE_TYPE (shadow_ptr_types[0]))
	return false;
      return true;
    }
  if (!VAR_P (decl)
      /* TLS vars aren't statically protectable.  */
      || DECL_THREAD_LOCAL_P (decl)
      /* Externs will be protected elsewhere.  */
      || DECL_EXTERNAL (decl)
      || !DECL_RTL_SET_P (decl)
      /* Comdat vars pose an ABI problem, we can't know if
	 the var that is selected by the linker will have
	 padding or not.  */
      || DECL_ONE_ONLY (decl)
      /* Similarly for common vars.  People can use -fno-common.
	 Note: Linux kernel is built with -fno-common, so we do instrument
	 globals there even if it is C.  */
      || (DECL_COMMON (decl) && TREE_PUBLIC (decl))
      /* Don't protect if using user section, often vars placed
	 into user section from multiple TUs are then assumed
	 to be an array of such vars, putting padding in there
	 breaks this assumption.  */
      || (DECL_SECTION_NAME (decl) != NULL
	  && !symtab_node::get (decl)->implicit_section
	  && !section_sanitized_p (DECL_SECTION_NAME (decl)))
      || DECL_SIZE (decl) == 0
      || ASAN_RED_ZONE_SIZE * BITS_PER_UNIT > MAX_OFILE_ALIGNMENT
      || !valid_constant_size_p (DECL_SIZE_UNIT (decl))
      || DECL_ALIGN_UNIT (decl) > 2 * ASAN_RED_ZONE_SIZE
      || TREE_TYPE (decl) == ubsan_get_source_location_type ()
      || is_odr_indicator (decl))
    return false;

  rtl = DECL_RTL (decl);
  if (!MEM_P (rtl) || GET_CODE (XEXP (rtl, 0)) != SYMBOL_REF)
    return false;
  symbol = XEXP (rtl, 0);

  if (CONSTANT_POOL_ADDRESS_P (symbol)
      || TREE_CONSTANT_POOL_ADDRESS_P (symbol))
    return false;

  if (lookup_attribute ("weakref", DECL_ATTRIBUTES (decl)))
    return false;

  if (!TARGET_SUPPORTS_ALIASES && asan_needs_local_alias (decl))
    return false;

  return true;
}

/* Construct a function tree for __asan_report_{load,store}{1,2,4,8,16,_n}.
   IS_STORE is either 1 (for a store) or 0 (for a load).  */

static tree
report_error_func (bool is_store, bool recover_p, HOST_WIDE_INT size_in_bytes,
		   int *nargs)
{
  static enum built_in_function report[2][2][6]
    = { { { BUILT_IN_ASAN_REPORT_LOAD1, BUILT_IN_ASAN_REPORT_LOAD2,
	    BUILT_IN_ASAN_REPORT_LOAD4, BUILT_IN_ASAN_REPORT_LOAD8,
	    BUILT_IN_ASAN_REPORT_LOAD16, BUILT_IN_ASAN_REPORT_LOAD_N },
	  { BUILT_IN_ASAN_REPORT_STORE1, BUILT_IN_ASAN_REPORT_STORE2,
	    BUILT_IN_ASAN_REPORT_STORE4, BUILT_IN_ASAN_REPORT_STORE8,
	    BUILT_IN_ASAN_REPORT_STORE16, BUILT_IN_ASAN_REPORT_STORE_N } },
	{ { BUILT_IN_ASAN_REPORT_LOAD1_NOABORT,
	    BUILT_IN_ASAN_REPORT_LOAD2_NOABORT,
	    BUILT_IN_ASAN_REPORT_LOAD4_NOABORT,
	    BUILT_IN_ASAN_REPORT_LOAD8_NOABORT,
	    BUILT_IN_ASAN_REPORT_LOAD16_NOABORT,
	    BUILT_IN_ASAN_REPORT_LOAD_N_NOABORT },
	  { BUILT_IN_ASAN_REPORT_STORE1_NOABORT,
	    BUILT_IN_ASAN_REPORT_STORE2_NOABORT,
	    BUILT_IN_ASAN_REPORT_STORE4_NOABORT,
	    BUILT_IN_ASAN_REPORT_STORE8_NOABORT,
	    BUILT_IN_ASAN_REPORT_STORE16_NOABORT,
	    BUILT_IN_ASAN_REPORT_STORE_N_NOABORT } } };
  if (size_in_bytes == -1)
    {
      *nargs = 2;
      return builtin_decl_implicit (report[recover_p][is_store][5]);
    }
  *nargs = 1;
  int size_log2 = exact_log2 (size_in_bytes);
  return builtin_decl_implicit (report[recover_p][is_store][size_log2]);
}

/* Construct a function tree for __asan_{load,store}{1,2,4,8,16,_n}.
   IS_STORE is either 1 (for a store) or 0 (for a load).  */

static tree
check_func (bool is_store, bool recover_p, HOST_WIDE_INT size_in_bytes,
	    int *nargs)
{
  static enum built_in_function check[2][2][6]
    = { { { BUILT_IN_ASAN_LOAD1, BUILT_IN_ASAN_LOAD2,
	    BUILT_IN_ASAN_LOAD4, BUILT_IN_ASAN_LOAD8,
	    BUILT_IN_ASAN_LOAD16, BUILT_IN_ASAN_LOADN },
	  { BUILT_IN_ASAN_STORE1, BUILT_IN_ASAN_STORE2,
	    BUILT_IN_ASAN_STORE4, BUILT_IN_ASAN_STORE8,
	    BUILT_IN_ASAN_STORE16, BUILT_IN_ASAN_STOREN } },
	{ { BUILT_IN_ASAN_LOAD1_NOABORT,
	    BUILT_IN_ASAN_LOAD2_NOABORT,
	    BUILT_IN_ASAN_LOAD4_NOABORT,
	    BUILT_IN_ASAN_LOAD8_NOABORT,
	    BUILT_IN_ASAN_LOAD16_NOABORT,
	    BUILT_IN_ASAN_LOADN_NOABORT },
	  { BUILT_IN_ASAN_STORE1_NOABORT,
	    BUILT_IN_ASAN_STORE2_NOABORT,
	    BUILT_IN_ASAN_STORE4_NOABORT,
	    BUILT_IN_ASAN_STORE8_NOABORT,
	    BUILT_IN_ASAN_STORE16_NOABORT,
	    BUILT_IN_ASAN_STOREN_NOABORT } } };
  if (size_in_bytes == -1)
    {
      *nargs = 2;
      return builtin_decl_implicit (check[recover_p][is_store][5]);
    }
  *nargs = 1;
  int size_log2 = exact_log2 (size_in_bytes);
  return builtin_decl_implicit (check[recover_p][is_store][size_log2]);
}

/* Split the current basic block and create a condition statement
   insertion point right before or after the statement pointed to by
   ITER.  Return an iterator to the point at which the caller might
   safely insert the condition statement.

   THEN_BLOCK must be set to the address of an uninitialized instance
   of basic_block.  The function will then set *THEN_BLOCK to the
   'then block' of the condition statement to be inserted by the
   caller.

   If CREATE_THEN_FALLTHRU_EDGE is false, no edge will be created from
   *THEN_BLOCK to *FALLTHROUGH_BLOCK.

   Similarly, the function will set *FALLTRHOUGH_BLOCK to the 'else
   block' of the condition statement to be inserted by the caller.

   Note that *FALLTHROUGH_BLOCK is a new block that contains the
   statements starting from *ITER, and *THEN_BLOCK is a new empty
   block.

   *ITER is adjusted to point to always point to the first statement
    of the basic block * FALLTHROUGH_BLOCK.  That statement is the
    same as what ITER was pointing to prior to calling this function,
    if BEFORE_P is true; otherwise, it is its following statement.  */

gimple_stmt_iterator
create_cond_insert_point (gimple_stmt_iterator *iter,
			  bool before_p,
			  bool then_more_likely_p,
			  bool create_then_fallthru_edge,
			  basic_block *then_block,
			  basic_block *fallthrough_block)
{
  gimple_stmt_iterator gsi = *iter;

  if (!gsi_end_p (gsi) && before_p)
    gsi_prev (&gsi);

  basic_block cur_bb = gsi_bb (*iter);

  edge e = split_block (cur_bb, gsi_stmt (gsi));

  /* Get a hold on the 'condition block', the 'then block' and the
     'else block'.  */
  basic_block cond_bb = e->src;
  basic_block fallthru_bb = e->dest;
  basic_block then_bb = create_empty_bb (cond_bb);
  if (current_loops)
    {
      add_bb_to_loop (then_bb, cond_bb->loop_father);
      loops_state_set (LOOPS_NEED_FIXUP);
    }

  /* Set up the newly created 'then block'.  */
  e = make_edge (cond_bb, then_bb, EDGE_TRUE_VALUE);
  profile_probability fallthrough_probability
    = then_more_likely_p
    ? profile_probability::very_unlikely ()
    : profile_probability::very_likely ();
  e->probability = fallthrough_probability.invert ();
  if (create_then_fallthru_edge)
    make_single_succ_edge (then_bb, fallthru_bb, EDGE_FALLTHRU);

  /* Set up the fallthrough basic block.  */
  e = find_edge (cond_bb, fallthru_bb);
  e->flags = EDGE_FALSE_VALUE;
  e->count = cond_bb->count;
  e->probability = fallthrough_probability;

  /* Update dominance info for the newly created then_bb; note that
     fallthru_bb's dominance info has already been updated by
     split_bock.  */
  if (dom_info_available_p (CDI_DOMINATORS))
    set_immediate_dominator (CDI_DOMINATORS, then_bb, cond_bb);

  *then_block = then_bb;
  *fallthrough_block = fallthru_bb;
  *iter = gsi_start_bb (fallthru_bb);

  return gsi_last_bb (cond_bb);
}

/* Insert an if condition followed by a 'then block' right before the
   statement pointed to by ITER.  The fallthrough block -- which is the
   else block of the condition as well as the destination of the
   outcoming edge of the 'then block' -- starts with the statement
   pointed to by ITER.

   COND is the condition of the if.

   If THEN_MORE_LIKELY_P is true, the probability of the edge to the
   'then block' is higher than the probability of the edge to the
   fallthrough block.

   Upon completion of the function, *THEN_BB is set to the newly
   inserted 'then block' and similarly, *FALLTHROUGH_BB is set to the
   fallthrough block.

   *ITER is adjusted to still point to the same statement it was
   pointing to initially.  */

static void
insert_if_then_before_iter (gcond *cond,
			    gimple_stmt_iterator *iter,
			    bool then_more_likely_p,
			    basic_block *then_bb,
			    basic_block *fallthrough_bb)
{
  gimple_stmt_iterator cond_insert_point =
    create_cond_insert_point (iter,
			      /*before_p=*/true,
			      then_more_likely_p,
			      /*create_then_fallthru_edge=*/true,
			      then_bb,
			      fallthrough_bb);
  gsi_insert_after (&cond_insert_point, cond, GSI_NEW_STMT);
}

/* Build (base_addr >> ASAN_SHADOW_SHIFT) + asan_shadow_offset ().
   If RETURN_ADDRESS is set to true, return memory location instread
   of a value in the shadow memory.  */

static tree
build_shadow_mem_access (gimple_stmt_iterator *gsi, location_t location,
			 tree base_addr, tree shadow_ptr_type,
			 bool return_address = false)
{
  tree t, uintptr_type = TREE_TYPE (base_addr);
  tree shadow_type = TREE_TYPE (shadow_ptr_type);
  gimple *g;

  t = build_int_cst (uintptr_type, ASAN_SHADOW_SHIFT);
  g = gimple_build_assign (make_ssa_name (uintptr_type), RSHIFT_EXPR,
			   base_addr, t);
  gimple_set_location (g, location);
  gsi_insert_after (gsi, g, GSI_NEW_STMT);

  t = build_int_cst (uintptr_type, asan_shadow_offset ());
  g = gimple_build_assign (make_ssa_name (uintptr_type), PLUS_EXPR,
			   gimple_assign_lhs (g), t);
  gimple_set_location (g, location);
  gsi_insert_after (gsi, g, GSI_NEW_STMT);

  g = gimple_build_assign (make_ssa_name (shadow_ptr_type), NOP_EXPR,
			   gimple_assign_lhs (g));
  gimple_set_location (g, location);
  gsi_insert_after (gsi, g, GSI_NEW_STMT);

  if (!return_address)
    {
      t = build2 (MEM_REF, shadow_type, gimple_assign_lhs (g),
		  build_int_cst (shadow_ptr_type, 0));
      g = gimple_build_assign (make_ssa_name (shadow_type), MEM_REF, t);
      gimple_set_location (g, location);
      gsi_insert_after (gsi, g, GSI_NEW_STMT);
    }

  return gimple_assign_lhs (g);
}

/* BASE can already be an SSA_NAME; in that case, do not create a
   new SSA_NAME for it.  */

static tree
maybe_create_ssa_name (location_t loc, tree base, gimple_stmt_iterator *iter,
		       bool before_p)
{
  if (TREE_CODE (base) == SSA_NAME)
    return base;
  gimple *g = gimple_build_assign (make_ssa_name (TREE_TYPE (base)),
				  TREE_CODE (base), base);
  gimple_set_location (g, loc);
  if (before_p)
    gsi_insert_before (iter, g, GSI_SAME_STMT);
  else
    gsi_insert_after (iter, g, GSI_NEW_STMT);
  return gimple_assign_lhs (g);
}

/* LEN can already have necessary size and precision;
   in that case, do not create a new variable.  */

tree
maybe_cast_to_ptrmode (location_t loc, tree len, gimple_stmt_iterator *iter,
		       bool before_p)
{
  if (ptrofftype_p (len))
    return len;
  gimple *g = gimple_build_assign (make_ssa_name (pointer_sized_int_node),
				  NOP_EXPR, len);
  gimple_set_location (g, loc);
  if (before_p)
    gsi_insert_before (iter, g, GSI_SAME_STMT);
  else
    gsi_insert_after (iter, g, GSI_NEW_STMT);
  return gimple_assign_lhs (g);
}

/* Instrument the memory access instruction BASE.  Insert new
   statements before or after ITER.

   Note that the memory access represented by BASE can be either an
   SSA_NAME, or a non-SSA expression.  LOCATION is the source code
   location.  IS_STORE is TRUE for a store, FALSE for a load.
   BEFORE_P is TRUE for inserting the instrumentation code before
   ITER, FALSE for inserting it after ITER.  IS_SCALAR_ACCESS is TRUE
   for a scalar memory access and FALSE for memory region access.
   NON_ZERO_P is TRUE if memory region is guaranteed to have non-zero
   length.  ALIGN tells alignment of accessed memory object.

   START_INSTRUMENTED and END_INSTRUMENTED are TRUE if start/end of
   memory region have already been instrumented.

   If BEFORE_P is TRUE, *ITER is arranged to still point to the
   statement it was pointing to prior to calling this function,
   otherwise, it points to the statement logically following it.  */

static void
build_check_stmt (location_t loc, tree base, tree len,
		  HOST_WIDE_INT size_in_bytes, gimple_stmt_iterator *iter,
		  bool is_non_zero_len, bool before_p, bool is_store,
		  bool is_scalar_access, unsigned int align = 0)
{
  gimple_stmt_iterator gsi = *iter;
  gimple *g;

  gcc_assert (!(size_in_bytes > 0 && !is_non_zero_len));

  gsi = *iter;

  base = unshare_expr (base);
  base = maybe_create_ssa_name (loc, base, &gsi, before_p);

  if (len)
    {
      len = unshare_expr (len);
      len = maybe_cast_to_ptrmode (loc, len, iter, before_p);
    }
  else
    {
      gcc_assert (size_in_bytes != -1);
      len = build_int_cst (pointer_sized_int_node, size_in_bytes);
    }

  if (size_in_bytes > 1)
    {
      if ((size_in_bytes & (size_in_bytes - 1)) != 0
	  || size_in_bytes > 16)
	is_scalar_access = false;
      else if (align && align < size_in_bytes * BITS_PER_UNIT)
	{
	  /* On non-strict alignment targets, if
	     16-byte access is just 8-byte aligned,
	     this will result in misaligned shadow
	     memory 2 byte load, but otherwise can
	     be handled using one read.  */
	  if (size_in_bytes != 16
	      || STRICT_ALIGNMENT
	      || align < 8 * BITS_PER_UNIT)
	    is_scalar_access = false;
	}
    }

  HOST_WIDE_INT flags = 0;
  if (is_store)
    flags |= ASAN_CHECK_STORE;
  if (is_non_zero_len)
    flags |= ASAN_CHECK_NON_ZERO_LEN;
  if (is_scalar_access)
    flags |= ASAN_CHECK_SCALAR_ACCESS;

  g = gimple_build_call_internal (IFN_ASAN_CHECK, 4,
				  build_int_cst (integer_type_node, flags),
				  base, len,
				  build_int_cst (integer_type_node,
						 align / BITS_PER_UNIT));
  gimple_set_location (g, loc);
  if (before_p)
    gsi_insert_before (&gsi, g, GSI_SAME_STMT);
  else
    {
      gsi_insert_after (&gsi, g, GSI_NEW_STMT);
      gsi_next (&gsi);
      *iter = gsi;
    }
}

/* If T represents a memory access, add instrumentation code before ITER.
   LOCATION is source code location.
   IS_STORE is either TRUE (for a store) or FALSE (for a load).  */

static void
instrument_derefs (gimple_stmt_iterator *iter, tree t,
		   location_t location, bool is_store)
{
  if (is_store && !ASAN_INSTRUMENT_WRITES)
    return;
  if (!is_store && !ASAN_INSTRUMENT_READS)
    return;

  tree type, base;
  HOST_WIDE_INT size_in_bytes;
  if (location == UNKNOWN_LOCATION)
    location = EXPR_LOCATION (t);

  type = TREE_TYPE (t);
  switch (TREE_CODE (t))
    {
    case ARRAY_REF:
    case COMPONENT_REF:
    case INDIRECT_REF:
    case MEM_REF:
    case VAR_DECL:
    case BIT_FIELD_REF:
      break;
      /* FALLTHRU */
    default:
      return;
    }

  size_in_bytes = int_size_in_bytes (type);
  if (size_in_bytes <= 0)
    return;

  HOST_WIDE_INT bitsize, bitpos;
  tree offset;
  machine_mode mode;
  int unsignedp, reversep, volatilep = 0;
  tree inner = get_inner_reference (t, &bitsize, &bitpos, &offset, &mode,
				    &unsignedp, &reversep, &volatilep);

  if (TREE_CODE (t) == COMPONENT_REF
      && DECL_BIT_FIELD_REPRESENTATIVE (TREE_OPERAND (t, 1)) != NULL_TREE)
    {
      tree repr = DECL_BIT_FIELD_REPRESENTATIVE (TREE_OPERAND (t, 1));
      instrument_derefs (iter, build3 (COMPONENT_REF, TREE_TYPE (repr),
				       TREE_OPERAND (t, 0), repr,
				       TREE_OPERAND (t, 2)),
			 location, is_store);
      return;
    }

  if (bitpos % BITS_PER_UNIT
      || bitsize != size_in_bytes * BITS_PER_UNIT)
    return;

  if (VAR_P (inner) && DECL_HARD_REGISTER (inner))
    return;

  if (VAR_P (inner)
      && offset == NULL_TREE
      && bitpos >= 0
      && DECL_SIZE (inner)
      && tree_fits_shwi_p (DECL_SIZE (inner))
      && bitpos + bitsize <= tree_to_shwi (DECL_SIZE (inner)))
    {
      if (DECL_THREAD_LOCAL_P (inner))
	return;
      if (!ASAN_GLOBALS && is_global_var (inner))
        return;
      if (!TREE_STATIC (inner))
	{
	  /* Automatic vars in the current function will be always
	     accessible.  */
	  if (decl_function_context (inner) == current_function_decl
	      && (!asan_sanitize_use_after_scope ()
		  || !TREE_ADDRESSABLE (inner)))
	    return;
	}
      /* Always instrument external vars, they might be dynamically
	 initialized.  */
      else if (!DECL_EXTERNAL (inner))
	{
	  /* For static vars if they are known not to be dynamically
	     initialized, they will be always accessible.  */
	  varpool_node *vnode = varpool_node::get (inner);
	  if (vnode && !vnode->dynamically_initialized)
	    return;
	}
    }

  base = build_fold_addr_expr (t);
  if (!has_mem_ref_been_instrumented (base, size_in_bytes))
    {
      unsigned int align = get_object_alignment (t);
      build_check_stmt (location, base, NULL_TREE, size_in_bytes, iter,
			/*is_non_zero_len*/size_in_bytes > 0, /*before_p=*/true,
			is_store, /*is_scalar_access*/true, align);
      update_mem_ref_hash_table (base, size_in_bytes);
      update_mem_ref_hash_table (t, size_in_bytes);
    }

}

/*  Insert a memory reference into the hash table if access length
    can be determined in compile time.  */

static void
maybe_update_mem_ref_hash_table (tree base, tree len)
{
  if (!POINTER_TYPE_P (TREE_TYPE (base))
      || !INTEGRAL_TYPE_P (TREE_TYPE (len)))
    return;

  HOST_WIDE_INT size_in_bytes = tree_fits_shwi_p (len) ? tree_to_shwi (len) : -1;

  if (size_in_bytes != -1)
    update_mem_ref_hash_table (base, size_in_bytes);
}

/* Instrument an access to a contiguous memory region that starts at
   the address pointed to by BASE, over a length of LEN (expressed in
   the sizeof (*BASE) bytes).  ITER points to the instruction before
   which the instrumentation instructions must be inserted.  LOCATION
   is the source location that the instrumentation instructions must
   have.  If IS_STORE is true, then the memory access is a store;
   otherwise, it's a load.  */

static void
instrument_mem_region_access (tree base, tree len,
			      gimple_stmt_iterator *iter,
			      location_t location, bool is_store)
{
  if (!POINTER_TYPE_P (TREE_TYPE (base))
      || !INTEGRAL_TYPE_P (TREE_TYPE (len))
      || integer_zerop (len))
    return;

  HOST_WIDE_INT size_in_bytes = tree_fits_shwi_p (len) ? tree_to_shwi (len) : -1;

  if ((size_in_bytes == -1)
      || !has_mem_ref_been_instrumented (base, size_in_bytes))
    {
      build_check_stmt (location, base, len, size_in_bytes, iter,
			/*is_non_zero_len*/size_in_bytes > 0, /*before_p*/true,
			is_store, /*is_scalar_access*/false, /*align*/0);
    }

  maybe_update_mem_ref_hash_table (base, len);
  *iter = gsi_for_stmt (gsi_stmt (*iter));
}

/* Instrument the call to a built-in memory access function that is
   pointed to by the iterator ITER.

   Upon completion, return TRUE iff *ITER has been advanced to the
   statement following the one it was originally pointing to.  */

static bool
instrument_builtin_call (gimple_stmt_iterator *iter)
{
  if (!ASAN_MEMINTRIN)
    return false;

  bool iter_advanced_p = false;
  gcall *call = as_a <gcall *> (gsi_stmt (*iter));

  gcc_checking_assert (gimple_call_builtin_p (call, BUILT_IN_NORMAL));

  location_t loc = gimple_location (call);

  asan_mem_ref src0, src1, dest;
  asan_mem_ref_init (&src0, NULL, 1);
  asan_mem_ref_init (&src1, NULL, 1);
  asan_mem_ref_init (&dest, NULL, 1);

  tree src0_len = NULL_TREE, src1_len = NULL_TREE, dest_len = NULL_TREE;
  bool src0_is_store = false, src1_is_store = false, dest_is_store = false,
    dest_is_deref = false, intercepted_p = true;

  if (get_mem_refs_of_builtin_call (call,
				    &src0, &src0_len, &src0_is_store,
				    &src1, &src1_len, &src1_is_store,
				    &dest, &dest_len, &dest_is_store,
				    &dest_is_deref, &intercepted_p, iter))
    {
      if (dest_is_deref)
	{
	  instrument_derefs (iter, dest.start, loc, dest_is_store);
	  gsi_next (iter);
	  iter_advanced_p = true;
	}
      else if (!intercepted_p
	       && (src0_len || src1_len || dest_len))
	{
	  if (src0.start != NULL_TREE)
	    instrument_mem_region_access (src0.start, src0_len,
					  iter, loc, /*is_store=*/false);
	  if (src1.start != NULL_TREE)
	    instrument_mem_region_access (src1.start, src1_len,
					  iter, loc, /*is_store=*/false);
	  if (dest.start != NULL_TREE)
	    instrument_mem_region_access (dest.start, dest_len,
					  iter, loc, /*is_store=*/true);

	  *iter = gsi_for_stmt (call);
	  gsi_next (iter);
	  iter_advanced_p = true;
	}
      else
	{
	  if (src0.start != NULL_TREE)
	    maybe_update_mem_ref_hash_table (src0.start, src0_len);
	  if (src1.start != NULL_TREE)
	    maybe_update_mem_ref_hash_table (src1.start, src1_len);
	  if (dest.start != NULL_TREE)
	    maybe_update_mem_ref_hash_table (dest.start, dest_len);
	}
    }
  return iter_advanced_p;
}

/*  Instrument the assignment statement ITER if it is subject to
    instrumentation.  Return TRUE iff instrumentation actually
    happened.  In that case, the iterator ITER is advanced to the next
    logical expression following the one initially pointed to by ITER,
    and the relevant memory reference that which access has been
    instrumented is added to the memory references hash table.  */

static bool
maybe_instrument_assignment (gimple_stmt_iterator *iter)
{
  gimple *s = gsi_stmt (*iter);

  gcc_assert (gimple_assign_single_p (s));

  tree ref_expr = NULL_TREE;
  bool is_store, is_instrumented = false;

  if (gimple_store_p (s))
    {
      ref_expr = gimple_assign_lhs (s);
      is_store = true;
      instrument_derefs (iter, ref_expr,
			 gimple_location (s),
			 is_store);
      is_instrumented = true;
    }

  if (gimple_assign_load_p (s))
    {
      ref_expr = gimple_assign_rhs1 (s);
      is_store = false;
      instrument_derefs (iter, ref_expr,
			 gimple_location (s),
			 is_store);
      is_instrumented = true;
    }

  if (is_instrumented)
    gsi_next (iter);

  return is_instrumented;
}

/* Instrument the function call pointed to by the iterator ITER, if it
   is subject to instrumentation.  At the moment, the only function
   calls that are instrumented are some built-in functions that access
   memory.  Look at instrument_builtin_call to learn more.

   Upon completion return TRUE iff *ITER was advanced to the statement
   following the one it was originally pointing to.  */

static bool
maybe_instrument_call (gimple_stmt_iterator *iter)
{
  gimple *stmt = gsi_stmt (*iter);
  bool is_builtin = gimple_call_builtin_p (stmt, BUILT_IN_NORMAL);

  if (is_builtin && instrument_builtin_call (iter))
    return true;

  if (gimple_call_noreturn_p (stmt))
    {
      if (is_builtin)
	{
	  tree callee = gimple_call_fndecl (stmt);
	  switch (DECL_FUNCTION_CODE (callee))
	    {
	    case BUILT_IN_UNREACHABLE:
	    case BUILT_IN_TRAP:
	      /* Don't instrument these.  */
	      return false;
	    default:
	      break;
	    }
	}
      tree decl = builtin_decl_implicit (BUILT_IN_ASAN_HANDLE_NO_RETURN);
      gimple *g = gimple_build_call (decl, 0);
      gimple_set_location (g, gimple_location (stmt));
      gsi_insert_before (iter, g, GSI_SAME_STMT);
    }

  bool instrumented = false;
  if (gimple_store_p (stmt))
    {
      tree ref_expr = gimple_call_lhs (stmt);
      instrument_derefs (iter, ref_expr,
			 gimple_location (stmt),
			 /*is_store=*/true);

      instrumented = true;
    }

  /* Walk through gimple_call arguments and check them id needed.  */
  unsigned args_num = gimple_call_num_args (stmt);
  for (unsigned i = 0; i < args_num; ++i)
    {
      tree arg = gimple_call_arg (stmt, i);
      /* If ARG is not a non-aggregate register variable, compiler in general
	 creates temporary for it and pass it as argument to gimple call.
	 But in some cases, e.g. when we pass by value a small structure that
	 fits to register, compiler can avoid extra overhead by pulling out
	 these temporaries.  In this case, we should check the argument.  */
      if (!is_gimple_reg (arg) && !is_gimple_min_invariant (arg))
	{
	  instrument_derefs (iter, arg,
			     gimple_location (stmt),
			     /*is_store=*/false);
	  instrumented = true;
	}
    }
  if (instrumented)
    gsi_next (iter);
  return instrumented;
}

/* Walk each instruction of all basic block and instrument those that
   represent memory references: loads, stores, or function calls.
   In a given basic block, this function avoids instrumenting memory
   references that have already been instrumented.  */

static void
transform_statements (void)
{
  basic_block bb, last_bb = NULL;
  gimple_stmt_iterator i;
  int saved_last_basic_block = last_basic_block_for_fn (cfun);

  FOR_EACH_BB_FN (bb, cfun)
    {
      basic_block prev_bb = bb;

      if (bb->index >= saved_last_basic_block) continue;

      /* Flush the mem ref hash table, if current bb doesn't have
	 exactly one predecessor, or if that predecessor (skipping
	 over asan created basic blocks) isn't the last processed
	 basic block.  Thus we effectively flush on extended basic
	 block boundaries.  */
      while (single_pred_p (prev_bb))
	{
	  prev_bb = single_pred (prev_bb);
	  if (prev_bb->index < saved_last_basic_block)
	    break;
	}
      if (prev_bb != last_bb)
	empty_mem_ref_hash_table ();
      last_bb = bb;

      for (i = gsi_start_bb (bb); !gsi_end_p (i);)
	{
	  gimple *s = gsi_stmt (i);

	  if (has_stmt_been_instrumented_p (s))
	    gsi_next (&i);
	  else if (gimple_assign_single_p (s)
		   && !gimple_clobber_p (s)
		   && maybe_instrument_assignment (&i))
	    /*  Nothing to do as maybe_instrument_assignment advanced
		the iterator I.  */;
	  else if (is_gimple_call (s) && maybe_instrument_call (&i))
	    /*  Nothing to do as maybe_instrument_call
		advanced the iterator I.  */;
	  else
	    {
	      /* No instrumentation happened.

		 If the current instruction is a function call that
		 might free something, let's forget about the memory
		 references that got instrumented.  Otherwise we might
		 miss some instrumentation opportunities.  Do the same
		 for a ASAN_MARK poisoning internal function.  */
	      if (is_gimple_call (s)
		  && (!nonfreeing_call_p (s)
		      || asan_mark_p (s, ASAN_MARK_POISON)))
		empty_mem_ref_hash_table ();

	      gsi_next (&i);
	    }
	}
    }
  free_mem_ref_resources ();
}

/* Build
   __asan_before_dynamic_init (module_name)
   or
   __asan_after_dynamic_init ()
   call.  */

tree
asan_dynamic_init_call (bool after_p)
{
  if (shadow_ptr_types[0] == NULL_TREE)
    asan_init_shadow_ptr_types ();

  tree fn = builtin_decl_implicit (after_p
				   ? BUILT_IN_ASAN_AFTER_DYNAMIC_INIT
				   : BUILT_IN_ASAN_BEFORE_DYNAMIC_INIT);
  tree module_name_cst = NULL_TREE;
  if (!after_p)
    {
      pretty_printer module_name_pp;
      pp_string (&module_name_pp, main_input_filename);

      module_name_cst = asan_pp_string (&module_name_pp);
      module_name_cst = fold_convert (const_ptr_type_node,
				      module_name_cst);
    }

  return build_call_expr (fn, after_p ? 0 : 1, module_name_cst);
}

/* Build
   struct __asan_global
   {
     const void *__beg;
     uptr __size;
     uptr __size_with_redzone;
     const void *__name;
     const void *__module_name;
     uptr __has_dynamic_init;
     __asan_global_source_location *__location;
     char *__odr_indicator;
   } type.  */

static tree
asan_global_struct (void)
{
  static const char *field_names[]
    = { "__beg", "__size", "__size_with_redzone",
	"__name", "__module_name", "__has_dynamic_init", "__location",
	"__odr_indicator" };
  tree fields[ARRAY_SIZE (field_names)], ret;
  unsigned i;

  ret = make_node (RECORD_TYPE);
  for (i = 0; i < ARRAY_SIZE (field_names); i++)
    {
      fields[i]
	= build_decl (UNKNOWN_LOCATION, FIELD_DECL,
		      get_identifier (field_names[i]),
		      (i == 0 || i == 3) ? const_ptr_type_node
		      : pointer_sized_int_node);
      DECL_CONTEXT (fields[i]) = ret;
      if (i)
	DECL_CHAIN (fields[i - 1]) = fields[i];
    }
  tree type_decl = build_decl (input_location, TYPE_DECL,
			       get_identifier ("__asan_global"), ret);
  DECL_IGNORED_P (type_decl) = 1;
  DECL_ARTIFICIAL (type_decl) = 1;
  TYPE_FIELDS (ret) = fields[0];
  TYPE_NAME (ret) = type_decl;
  TYPE_STUB_DECL (ret) = type_decl;
  layout_type (ret);
  return ret;
}

/* Create and return odr indicator symbol for DECL.
   TYPE is __asan_global struct type as returned by asan_global_struct.  */

static tree
create_odr_indicator (tree decl, tree type)
{
  char *name;
  tree uptr = TREE_TYPE (DECL_CHAIN (TYPE_FIELDS (type)));
  tree decl_name
    = (HAS_DECL_ASSEMBLER_NAME_P (decl) ? DECL_ASSEMBLER_NAME (decl)
					: DECL_NAME (decl));
  /* DECL_NAME theoretically might be NULL.  Bail out with 0 in this case.  */
  if (decl_name == NULL_TREE)
    return build_int_cst (uptr, 0);
  const char *dname = IDENTIFIER_POINTER (decl_name);
  if (HAS_DECL_ASSEMBLER_NAME_P (decl))
    dname = targetm.strip_name_encoding (dname);
  size_t len = strlen (dname) + sizeof ("__odr_asan_");
  name = XALLOCAVEC (char, len);
  snprintf (name, len, "__odr_asan_%s", dname);
#ifndef NO_DOT_IN_LABEL
  name[sizeof ("__odr_asan") - 1] = '.';
#elif !defined(NO_DOLLAR_IN_LABEL)
  name[sizeof ("__odr_asan") - 1] = '$';
#endif
  tree var = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier (name),
			 char_type_node);
  TREE_ADDRESSABLE (var) = 1;
  TREE_READONLY (var) = 0;
  TREE_THIS_VOLATILE (var) = 1;
  DECL_GIMPLE_REG_P (var) = 0;
  DECL_ARTIFICIAL (var) = 1;
  DECL_IGNORED_P (var) = 1;
  TREE_STATIC (var) = 1;
  TREE_PUBLIC (var) = 1;
  DECL_VISIBILITY (var) = DECL_VISIBILITY (decl);
  DECL_VISIBILITY_SPECIFIED (var) = DECL_VISIBILITY_SPECIFIED (decl);

  TREE_USED (var) = 1;
  tree ctor = build_constructor_va (TREE_TYPE (var), 1, NULL_TREE,
				    build_int_cst (unsigned_type_node, 0));
  TREE_CONSTANT (ctor) = 1;
  TREE_STATIC (ctor) = 1;
  DECL_INITIAL (var) = ctor;
  DECL_ATTRIBUTES (var) = tree_cons (get_identifier ("asan odr indicator"),
				     NULL, DECL_ATTRIBUTES (var));
  make_decl_rtl (var);
  varpool_node::finalize_decl (var);
  return fold_convert (uptr, build_fold_addr_expr (var));
}

/* Return true if DECL, a global var, might be overridden and needs
   an additional odr indicator symbol.  */

static bool
asan_needs_odr_indicator_p (tree decl)
{
  /* Don't emit ODR indicators for kernel because:
     a) Kernel is written in C thus doesn't need ODR indicators.
     b) Some kernel code may have assumptions about symbols containing specific
        patterns in their names.  Since ODR indicators contain original names
        of symbols they are emitted for, these assumptions would be broken for
        ODR indicator symbols.  */
  return (!(flag_sanitize & SANITIZE_KERNEL_ADDRESS)
	  && !DECL_ARTIFICIAL (decl)
	  && !DECL_WEAK (decl)
	  && TREE_PUBLIC (decl));
}

/* Append description of a single global DECL into vector V.
   TYPE is __asan_global struct type as returned by asan_global_struct.  */

static void
asan_add_global (tree decl, tree type, vec<constructor_elt, va_gc> *v)
{
  tree init, uptr = TREE_TYPE (DECL_CHAIN (TYPE_FIELDS (type)));
  unsigned HOST_WIDE_INT size;
  tree str_cst, module_name_cst, refdecl = decl;
  vec<constructor_elt, va_gc> *vinner = NULL;

  pretty_printer asan_pp, module_name_pp;

  if (DECL_NAME (decl))
    pp_tree_identifier (&asan_pp, DECL_NAME (decl));
  else
    pp_string (&asan_pp, "<unknown>");
  str_cst = asan_pp_string (&asan_pp);

  pp_string (&module_name_pp, main_input_filename);
  module_name_cst = asan_pp_string (&module_name_pp);

  if (asan_needs_local_alias (decl))
    {
      char buf[20];
      ASM_GENERATE_INTERNAL_LABEL (buf, "LASAN", vec_safe_length (v) + 1);
      refdecl = build_decl (DECL_SOURCE_LOCATION (decl),
			    VAR_DECL, get_identifier (buf), TREE_TYPE (decl));
      TREE_ADDRESSABLE (refdecl) = TREE_ADDRESSABLE (decl);
      TREE_READONLY (refdecl) = TREE_READONLY (decl);
      TREE_THIS_VOLATILE (refdecl) = TREE_THIS_VOLATILE (decl);
      DECL_GIMPLE_REG_P (refdecl) = DECL_GIMPLE_REG_P (decl);
      DECL_ARTIFICIAL (refdecl) = DECL_ARTIFICIAL (decl);
      DECL_IGNORED_P (refdecl) = DECL_IGNORED_P (decl);
      TREE_STATIC (refdecl) = 1;
      TREE_PUBLIC (refdecl) = 0;
      TREE_USED (refdecl) = 1;
      assemble_alias (refdecl, DECL_ASSEMBLER_NAME (decl));
    }

  tree odr_indicator_ptr
    = (asan_needs_odr_indicator_p (decl) ? create_odr_indicator (decl, type)
					 : build_int_cst (uptr, 0));
  CONSTRUCTOR_APPEND_ELT (vinner, NULL_TREE,
			  fold_convert (const_ptr_type_node,
					build_fold_addr_expr (refdecl)));
  size = tree_to_uhwi (DECL_SIZE_UNIT (decl));
  CONSTRUCTOR_APPEND_ELT (vinner, NULL_TREE, build_int_cst (uptr, size));
  size += asan_red_zone_size (size);
  CONSTRUCTOR_APPEND_ELT (vinner, NULL_TREE, build_int_cst (uptr, size));
  CONSTRUCTOR_APPEND_ELT (vinner, NULL_TREE,
			  fold_convert (const_ptr_type_node, str_cst));
  CONSTRUCTOR_APPEND_ELT (vinner, NULL_TREE,
			  fold_convert (const_ptr_type_node, module_name_cst));
  varpool_node *vnode = varpool_node::get (decl);
  int has_dynamic_init = 0;
  /* FIXME: Enable initialization order fiasco detection in LTO mode once
     proper fix for PR 79061 will be applied.  */
  if (!in_lto_p)
    has_dynamic_init = vnode ? vnode->dynamically_initialized : 0;
  CONSTRUCTOR_APPEND_ELT (vinner, NULL_TREE,
			  build_int_cst (uptr, has_dynamic_init));
  tree locptr = NULL_TREE;
  location_t loc = DECL_SOURCE_LOCATION (decl);
  expanded_location xloc = expand_location (loc);
  if (xloc.file != NULL)
    {
      static int lasanloccnt = 0;
      char buf[25];
      ASM_GENERATE_INTERNAL_LABEL (buf, "LASANLOC", ++lasanloccnt);
      tree var = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier (buf),
			     ubsan_get_source_location_type ());
      TREE_STATIC (var) = 1;
      TREE_PUBLIC (var) = 0;
      DECL_ARTIFICIAL (var) = 1;
      DECL_IGNORED_P (var) = 1;
      pretty_printer filename_pp;
      pp_string (&filename_pp, xloc.file);
      tree str = asan_pp_string (&filename_pp);
      tree ctor = build_constructor_va (TREE_TYPE (var), 3,
					NULL_TREE, str, NULL_TREE,
					build_int_cst (unsigned_type_node,
						       xloc.line), NULL_TREE,
					build_int_cst (unsigned_type_node,
						       xloc.column));
      TREE_CONSTANT (ctor) = 1;
      TREE_STATIC (ctor) = 1;
      DECL_INITIAL (var) = ctor;
      varpool_node::finalize_decl (var);
      locptr = fold_convert (uptr, build_fold_addr_expr (var));
    }
  else
    locptr = build_int_cst (uptr, 0);
  CONSTRUCTOR_APPEND_ELT (vinner, NULL_TREE, locptr);
  CONSTRUCTOR_APPEND_ELT (vinner, NULL_TREE, odr_indicator_ptr);
  init = build_constructor (type, vinner);
  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, init);
}

/* Initialize sanitizer.def builtins if the FE hasn't initialized them.  */
void
initialize_sanitizer_builtins (void)
{
  tree decl;

  if (builtin_decl_implicit_p (BUILT_IN_ASAN_INIT))
    return;

  tree BT_FN_VOID = build_function_type_list (void_type_node, NULL_TREE);
  tree BT_FN_VOID_PTR
    = build_function_type_list (void_type_node, ptr_type_node, NULL_TREE);
  tree BT_FN_VOID_CONST_PTR
    = build_function_type_list (void_type_node, const_ptr_type_node, NULL_TREE);
  tree BT_FN_VOID_PTR_PTR
    = build_function_type_list (void_type_node, ptr_type_node,
				ptr_type_node, NULL_TREE);
  tree BT_FN_VOID_PTR_PTR_PTR
    = build_function_type_list (void_type_node, ptr_type_node,
				ptr_type_node, ptr_type_node, NULL_TREE);
  tree BT_FN_VOID_PTR_PTRMODE
    = build_function_type_list (void_type_node, ptr_type_node,
				pointer_sized_int_node, NULL_TREE);
  tree BT_FN_VOID_INT
    = build_function_type_list (void_type_node, integer_type_node, NULL_TREE);
  tree BT_FN_SIZE_CONST_PTR_INT
    = build_function_type_list (size_type_node, const_ptr_type_node,
				integer_type_node, NULL_TREE);

  tree BT_FN_VOID_UINT8_UINT8
    = build_function_type_list (void_type_node, unsigned_char_type_node,
				unsigned_char_type_node, NULL_TREE);
  tree BT_FN_VOID_UINT16_UINT16
    = build_function_type_list (void_type_node, uint16_type_node,
				uint16_type_node, NULL_TREE);
  tree BT_FN_VOID_UINT32_UINT32
    = build_function_type_list (void_type_node, uint32_type_node,
				uint32_type_node, NULL_TREE);
  tree BT_FN_VOID_UINT64_UINT64
    = build_function_type_list (void_type_node, uint64_type_node,
				uint64_type_node, NULL_TREE);
  tree BT_FN_VOID_FLOAT_FLOAT
    = build_function_type_list (void_type_node, float_type_node,
				float_type_node, NULL_TREE);
  tree BT_FN_VOID_DOUBLE_DOUBLE
    = build_function_type_list (void_type_node, double_type_node,
				double_type_node, NULL_TREE);
  tree BT_FN_VOID_UINT64_PTR
    = build_function_type_list (void_type_node, uint64_type_node,
				ptr_type_node, NULL_TREE);

  tree BT_FN_BOOL_VPTR_PTR_IX_INT_INT[5];
  tree BT_FN_IX_CONST_VPTR_INT[5];
  tree BT_FN_IX_VPTR_IX_INT[5];
  tree BT_FN_VOID_VPTR_IX_INT[5];
  tree vptr
    = build_pointer_type (build_qualified_type (void_type_node,
						TYPE_QUAL_VOLATILE));
  tree cvptr
    = build_pointer_type (build_qualified_type (void_type_node,
						TYPE_QUAL_VOLATILE
						|TYPE_QUAL_CONST));
  tree boolt
    = lang_hooks.types.type_for_size (BOOL_TYPE_SIZE, 1);
  int i;
  for (i = 0; i < 5; i++)
    {
      tree ix = build_nonstandard_integer_type (BITS_PER_UNIT * (1 << i), 1);
      BT_FN_BOOL_VPTR_PTR_IX_INT_INT[i]
	= build_function_type_list (boolt, vptr, ptr_type_node, ix,
				    integer_type_node, integer_type_node,
				    NULL_TREE);
      BT_FN_IX_CONST_VPTR_INT[i]
	= build_function_type_list (ix, cvptr, integer_type_node, NULL_TREE);
      BT_FN_IX_VPTR_IX_INT[i]
	= build_function_type_list (ix, vptr, ix, integer_type_node,
				    NULL_TREE);
      BT_FN_VOID_VPTR_IX_INT[i]
	= build_function_type_list (void_type_node, vptr, ix,
				    integer_type_node, NULL_TREE);
    }
#define BT_FN_BOOL_VPTR_PTR_I1_INT_INT BT_FN_BOOL_VPTR_PTR_IX_INT_INT[0]
#define BT_FN_I1_CONST_VPTR_INT BT_FN_IX_CONST_VPTR_INT[0]
#define BT_FN_I1_VPTR_I1_INT BT_FN_IX_VPTR_IX_INT[0]
#define BT_FN_VOID_VPTR_I1_INT BT_FN_VOID_VPTR_IX_INT[0]
#define BT_FN_BOOL_VPTR_PTR_I2_INT_INT BT_FN_BOOL_VPTR_PTR_IX_INT_INT[1]
#define BT_FN_I2_CONST_VPTR_INT BT_FN_IX_CONST_VPTR_INT[1]
#define BT_FN_I2_VPTR_I2_INT BT_FN_IX_VPTR_IX_INT[1]
#define BT_FN_VOID_VPTR_I2_INT BT_FN_VOID_VPTR_IX_INT[1]
#define BT_FN_BOOL_VPTR_PTR_I4_INT_INT BT_FN_BOOL_VPTR_PTR_IX_INT_INT[2]
#define BT_FN_I4_CONST_VPTR_INT BT_FN_IX_CONST_VPTR_INT[2]
#define BT_FN_I4_VPTR_I4_INT BT_FN_IX_VPTR_IX_INT[2]
#define BT_FN_VOID_VPTR_I4_INT BT_FN_VOID_VPTR_IX_INT[2]
#define BT_FN_BOOL_VPTR_PTR_I8_INT_INT BT_FN_BOOL_VPTR_PTR_IX_INT_INT[3]
#define BT_FN_I8_CONST_VPTR_INT BT_FN_IX_CONST_VPTR_INT[3]
#define BT_FN_I8_VPTR_I8_INT BT_FN_IX_VPTR_IX_INT[3]
#define BT_FN_VOID_VPTR_I8_INT BT_FN_VOID_VPTR_IX_INT[3]
#define BT_FN_BOOL_VPTR_PTR_I16_INT_INT BT_FN_BOOL_VPTR_PTR_IX_INT_INT[4]
#define BT_FN_I16_CONST_VPTR_INT BT_FN_IX_CONST_VPTR_INT[4]
#define BT_FN_I16_VPTR_I16_INT BT_FN_IX_VPTR_IX_INT[4]
#define BT_FN_VOID_VPTR_I16_INT BT_FN_VOID_VPTR_IX_INT[4]
#undef ATTR_NOTHROW_LEAF_LIST
#define ATTR_NOTHROW_LEAF_LIST ECF_NOTHROW | ECF_LEAF
#undef ATTR_TMPURE_NOTHROW_LEAF_LIST
#define ATTR_TMPURE_NOTHROW_LEAF_LIST ECF_TM_PURE | ATTR_NOTHROW_LEAF_LIST
#undef ATTR_NORETURN_NOTHROW_LEAF_LIST
#define ATTR_NORETURN_NOTHROW_LEAF_LIST ECF_NORETURN | ATTR_NOTHROW_LEAF_LIST
#undef ATTR_CONST_NORETURN_NOTHROW_LEAF_LIST
#define ATTR_CONST_NORETURN_NOTHROW_LEAF_LIST \
  ECF_CONST | ATTR_NORETURN_NOTHROW_LEAF_LIST
#undef ATTR_TMPURE_NORETURN_NOTHROW_LEAF_LIST
#define ATTR_TMPURE_NORETURN_NOTHROW_LEAF_LIST \
  ECF_TM_PURE | ATTR_NORETURN_NOTHROW_LEAF_LIST
#undef ATTR_COLD_NOTHROW_LEAF_LIST
#define ATTR_COLD_NOTHROW_LEAF_LIST \
  /* ECF_COLD missing */ ATTR_NOTHROW_LEAF_LIST
#undef ATTR_COLD_NORETURN_NOTHROW_LEAF_LIST
#define ATTR_COLD_NORETURN_NOTHROW_LEAF_LIST \
  /* ECF_COLD missing */ ATTR_NORETURN_NOTHROW_LEAF_LIST
#undef ATTR_COLD_CONST_NORETURN_NOTHROW_LEAF_LIST
#define ATTR_COLD_CONST_NORETURN_NOTHROW_LEAF_LIST \
  /* ECF_COLD missing */ ATTR_CONST_NORETURN_NOTHROW_LEAF_LIST
#undef ATTR_PURE_NOTHROW_LEAF_LIST
#define ATTR_PURE_NOTHROW_LEAF_LIST ECF_PURE | ATTR_NOTHROW_LEAF_LIST
#undef DEF_BUILTIN_STUB
#define DEF_BUILTIN_STUB(ENUM, NAME)
#undef DEF_SANITIZER_BUILTIN
#define DEF_SANITIZER_BUILTIN(ENUM, NAME, TYPE, ATTRS) \
  do {									\
    decl = add_builtin_function ("__builtin_" NAME, TYPE, ENUM,		\
				 BUILT_IN_NORMAL, NAME, NULL_TREE);	\
    set_call_expr_flags (decl, ATTRS);					\
    set_builtin_decl (ENUM, decl, true);				\
  } while (0);

#include "sanitizer.def"

  /* -fsanitize=object-size uses __builtin_object_size, but that might
     not be available for e.g. Fortran at this point.  We use
     DEF_SANITIZER_BUILTIN here only as a convenience macro.  */
  if ((flag_sanitize & SANITIZE_OBJECT_SIZE)
      && !builtin_decl_implicit_p (BUILT_IN_OBJECT_SIZE))
    DEF_SANITIZER_BUILTIN (BUILT_IN_OBJECT_SIZE, "object_size",
			   BT_FN_SIZE_CONST_PTR_INT,
			   ATTR_PURE_NOTHROW_LEAF_LIST)

#undef DEF_SANITIZER_BUILTIN
#undef DEF_BUILTIN_STUB
}

/* Called via htab_traverse.  Count number of emitted
   STRING_CSTs in the constant hash table.  */

int
count_string_csts (constant_descriptor_tree **slot,
		   unsigned HOST_WIDE_INT *data)
{
  struct constant_descriptor_tree *desc = *slot;
  if (TREE_CODE (desc->value) == STRING_CST
      && TREE_ASM_WRITTEN (desc->value)
      && asan_protect_global (desc->value))
    ++*data;
  return 1;
}

/* Helper structure to pass two parameters to
   add_string_csts.  */

struct asan_add_string_csts_data
{
  tree type;
  vec<constructor_elt, va_gc> *v;
};

/* Called via hash_table::traverse.  Call asan_add_global
   on emitted STRING_CSTs from the constant hash table.  */

int
add_string_csts (constant_descriptor_tree **slot,
		 asan_add_string_csts_data *aascd)
{
  struct constant_descriptor_tree *desc = *slot;
  if (TREE_CODE (desc->value) == STRING_CST
      && TREE_ASM_WRITTEN (desc->value)
      && asan_protect_global (desc->value))
    {
      asan_add_global (SYMBOL_REF_DECL (XEXP (desc->rtl, 0)),
		       aascd->type, aascd->v);
    }
  return 1;
}

/* Needs to be GTY(()), because cgraph_build_static_cdtor may
   invoke ggc_collect.  */
static GTY(()) tree asan_ctor_statements;

/* Module-level instrumentation.
   - Insert __asan_init_vN() into the list of CTORs.
   - TODO: insert redzones around globals.
 */

void
asan_finish_file (void)
{
  varpool_node *vnode;
  unsigned HOST_WIDE_INT gcount = 0;

  if (shadow_ptr_types[0] == NULL_TREE)
    asan_init_shadow_ptr_types ();
  /* Avoid instrumenting code in the asan ctors/dtors.
     We don't need to insert padding after the description strings,
     nor after .LASAN* array.  */
  flag_sanitize &= ~SANITIZE_ADDRESS;

  /* For user-space we want asan constructors to run first.
     Linux kernel does not support priorities other than default, and the only
     other user of constructors is coverage. So we run with the default
     priority.  */
  int priority = flag_sanitize & SANITIZE_USER_ADDRESS
                 ? MAX_RESERVED_INIT_PRIORITY - 1 : DEFAULT_INIT_PRIORITY;

  if (flag_sanitize & SANITIZE_USER_ADDRESS)
    {
      tree fn = builtin_decl_implicit (BUILT_IN_ASAN_INIT);
      append_to_statement_list (build_call_expr (fn, 0), &asan_ctor_statements);
      fn = builtin_decl_implicit (BUILT_IN_ASAN_VERSION_MISMATCH_CHECK);
      append_to_statement_list (build_call_expr (fn, 0), &asan_ctor_statements);
    }
  FOR_EACH_DEFINED_VARIABLE (vnode)
    if (TREE_ASM_WRITTEN (vnode->decl)
	&& asan_protect_global (vnode->decl))
      ++gcount;
  hash_table<tree_descriptor_hasher> *const_desc_htab = constant_pool_htab ();
  const_desc_htab->traverse<unsigned HOST_WIDE_INT *, count_string_csts>
    (&gcount);
  if (gcount)
    {
      tree type = asan_global_struct (), var, ctor;
      tree dtor_statements = NULL_TREE;
      vec<constructor_elt, va_gc> *v;
      char buf[20];

      type = build_array_type_nelts (type, gcount);
      ASM_GENERATE_INTERNAL_LABEL (buf, "LASAN", 0);
      var = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier (buf),
			type);
      TREE_STATIC (var) = 1;
      TREE_PUBLIC (var) = 0;
      DECL_ARTIFICIAL (var) = 1;
      DECL_IGNORED_P (var) = 1;
      vec_alloc (v, gcount);
      FOR_EACH_DEFINED_VARIABLE (vnode)
	if (TREE_ASM_WRITTEN (vnode->decl)
	    && asan_protect_global (vnode->decl))
	  asan_add_global (vnode->decl, TREE_TYPE (type), v);
      struct asan_add_string_csts_data aascd;
      aascd.type = TREE_TYPE (type);
      aascd.v = v;
      const_desc_htab->traverse<asan_add_string_csts_data *, add_string_csts>
       	(&aascd);
      ctor = build_constructor (type, v);
      TREE_CONSTANT (ctor) = 1;
      TREE_STATIC (ctor) = 1;
      DECL_INITIAL (var) = ctor;
      varpool_node::finalize_decl (var);

      tree fn = builtin_decl_implicit (BUILT_IN_ASAN_REGISTER_GLOBALS);
      tree gcount_tree = build_int_cst (pointer_sized_int_node, gcount);
      append_to_statement_list (build_call_expr (fn, 2,
						 build_fold_addr_expr (var),
						 gcount_tree),
				&asan_ctor_statements);

      fn = builtin_decl_implicit (BUILT_IN_ASAN_UNREGISTER_GLOBALS);
      append_to_statement_list (build_call_expr (fn, 2,
						 build_fold_addr_expr (var),
						 gcount_tree),
				&dtor_statements);
      cgraph_build_static_cdtor ('D', dtor_statements, priority);
    }
  if (asan_ctor_statements)
    cgraph_build_static_cdtor ('I', asan_ctor_statements, priority);
  flag_sanitize |= SANITIZE_ADDRESS;
}

/* Poison or unpoison (depending on IS_CLOBBER variable) shadow memory based
   on SHADOW address.  Newly added statements will be added to ITER with
   given location LOC.  We mark SIZE bytes in shadow memory, where
   LAST_CHUNK_SIZE is greater than zero in situation where we are at the
   end of a variable.  */

static void
asan_store_shadow_bytes (gimple_stmt_iterator *iter, location_t loc,
			 tree shadow,
			 unsigned HOST_WIDE_INT base_addr_offset,
			 bool is_clobber, unsigned size,
			 unsigned last_chunk_size)
{
  tree shadow_ptr_type;

  switch (size)
    {
    case 1:
      shadow_ptr_type = shadow_ptr_types[0];
      break;
    case 2:
      shadow_ptr_type = shadow_ptr_types[1];
      break;
    case 4:
      shadow_ptr_type = shadow_ptr_types[2];
      break;
    default:
      gcc_unreachable ();
    }

  unsigned char c = (char) is_clobber ? ASAN_STACK_MAGIC_USE_AFTER_SCOPE : 0;
  unsigned HOST_WIDE_INT val = 0;
  unsigned last_pos = size;
  if (last_chunk_size && !is_clobber)
    last_pos = BYTES_BIG_ENDIAN ? 0 : size - 1;
  for (unsigned i = 0; i < size; ++i)
    {
      unsigned char shadow_c = c;
      if (i == last_pos)
	shadow_c = last_chunk_size;
      val |= (unsigned HOST_WIDE_INT) shadow_c << (BITS_PER_UNIT * i);
    }

  /* Handle last chunk in unpoisoning.  */
  tree magic = build_int_cst (TREE_TYPE (shadow_ptr_type), val);

  tree dest = build2 (MEM_REF, TREE_TYPE (shadow_ptr_type), shadow,
		      build_int_cst (shadow_ptr_type, base_addr_offset));

  gimple *g = gimple_build_assign (dest, magic);
  gimple_set_location (g, loc);
  gsi_insert_after (iter, g, GSI_NEW_STMT);
}

/* Expand the ASAN_MARK builtins.  */

bool
asan_expand_mark_ifn (gimple_stmt_iterator *iter)
{
  gimple *g = gsi_stmt (*iter);
  location_t loc = gimple_location (g);
  HOST_WIDE_INT flag = tree_to_shwi (gimple_call_arg (g, 0));
  bool is_poison = ((asan_mark_flags)flag) == ASAN_MARK_POISON;

  tree base = gimple_call_arg (g, 1);
  gcc_checking_assert (TREE_CODE (base) == ADDR_EXPR);
  tree decl = TREE_OPERAND (base, 0);

  /* For a nested function, we can have: ASAN_MARK (2, &FRAME.2.fp_input, 4) */
  if (TREE_CODE (decl) == COMPONENT_REF
      && DECL_NONLOCAL_FRAME (TREE_OPERAND (decl, 0)))
    decl = TREE_OPERAND (decl, 0);

  gcc_checking_assert (TREE_CODE (decl) == VAR_DECL);

  if (is_poison)
    {
      if (asan_handled_variables == NULL)
	asan_handled_variables = new hash_set<tree> (16);
      asan_handled_variables->add (decl);
    }
  tree len = gimple_call_arg (g, 2);

  gcc_assert (tree_fits_shwi_p (len));
  unsigned HOST_WIDE_INT size_in_bytes = tree_to_shwi (len);
  gcc_assert (size_in_bytes);

  g = gimple_build_assign (make_ssa_name (pointer_sized_int_node),
			   NOP_EXPR, base);
  gimple_set_location (g, loc);
  gsi_replace (iter, g, false);
  tree base_addr = gimple_assign_lhs (g);

  /* Generate direct emission if size_in_bytes is small.  */
  if (size_in_bytes <= ASAN_PARAM_USE_AFTER_SCOPE_DIRECT_EMISSION_THRESHOLD)
    {
      unsigned HOST_WIDE_INT shadow_size = shadow_mem_size (size_in_bytes);

      tree shadow = build_shadow_mem_access (iter, loc, base_addr,
					     shadow_ptr_types[0], true);

      for (unsigned HOST_WIDE_INT offset = 0; offset < shadow_size;)
	{
	  unsigned size = 1;
	  if (shadow_size - offset >= 4)
	    size = 4;
	  else if (shadow_size - offset >= 2)
	    size = 2;

	  unsigned HOST_WIDE_INT last_chunk_size = 0;
	  unsigned HOST_WIDE_INT s = (offset + size) * ASAN_SHADOW_GRANULARITY;
	  if (s > size_in_bytes)
	    last_chunk_size = ASAN_SHADOW_GRANULARITY - (s - size_in_bytes);

	  asan_store_shadow_bytes (iter, loc, shadow, offset, is_poison,
				   size, last_chunk_size);
	  offset += size;
	}
    }
  else
    {
      g = gimple_build_assign (make_ssa_name (pointer_sized_int_node),
			       NOP_EXPR, len);
      gimple_set_location (g, loc);
      gsi_insert_before (iter, g, GSI_SAME_STMT);
      tree sz_arg = gimple_assign_lhs (g);

      tree fun
	= builtin_decl_implicit (is_poison ? BUILT_IN_ASAN_POISON_STACK_MEMORY
				 : BUILT_IN_ASAN_UNPOISON_STACK_MEMORY);
      g = gimple_build_call (fun, 2, base_addr, sz_arg);
      gimple_set_location (g, loc);
      gsi_insert_after (iter, g, GSI_NEW_STMT);
    }

  return false;
}

/* Expand the ASAN_{LOAD,STORE} builtins.  */

bool
asan_expand_check_ifn (gimple_stmt_iterator *iter, bool use_calls)
{
  gimple *g = gsi_stmt (*iter);
  location_t loc = gimple_location (g);
  bool recover_p;
  if (flag_sanitize & SANITIZE_USER_ADDRESS)
    recover_p = (flag_sanitize_recover & SANITIZE_USER_ADDRESS) != 0;
  else
    recover_p = (flag_sanitize_recover & SANITIZE_KERNEL_ADDRESS) != 0;

  HOST_WIDE_INT flags = tree_to_shwi (gimple_call_arg (g, 0));
  gcc_assert (flags < ASAN_CHECK_LAST);
  bool is_scalar_access = (flags & ASAN_CHECK_SCALAR_ACCESS) != 0;
  bool is_store = (flags & ASAN_CHECK_STORE) != 0;
  bool is_non_zero_len = (flags & ASAN_CHECK_NON_ZERO_LEN) != 0;

  tree base = gimple_call_arg (g, 1);
  tree len = gimple_call_arg (g, 2);
  HOST_WIDE_INT align = tree_to_shwi (gimple_call_arg (g, 3));

  HOST_WIDE_INT size_in_bytes
    = is_scalar_access && tree_fits_shwi_p (len) ? tree_to_shwi (len) : -1;

  if (use_calls)
    {
      /* Instrument using callbacks.  */
      gimple *g = gimple_build_assign (make_ssa_name (pointer_sized_int_node),
				      NOP_EXPR, base);
      gimple_set_location (g, loc);
      gsi_insert_before (iter, g, GSI_SAME_STMT);
      tree base_addr = gimple_assign_lhs (g);

      int nargs;
      tree fun = check_func (is_store, recover_p, size_in_bytes, &nargs);
      if (nargs == 1)
	g = gimple_build_call (fun, 1, base_addr);
      else
	{
	  gcc_assert (nargs == 2);
	  g = gimple_build_assign (make_ssa_name (pointer_sized_int_node),
				   NOP_EXPR, len);
	  gimple_set_location (g, loc);
	  gsi_insert_before (iter, g, GSI_SAME_STMT);
	  tree sz_arg = gimple_assign_lhs (g);
	  g = gimple_build_call (fun, nargs, base_addr, sz_arg);
	}
      gimple_set_location (g, loc);
      gsi_replace (iter, g, false);
      return false;
    }

  HOST_WIDE_INT real_size_in_bytes = size_in_bytes == -1 ? 1 : size_in_bytes;

  tree shadow_ptr_type = shadow_ptr_types[real_size_in_bytes == 16 ? 1 : 0];
  tree shadow_type = TREE_TYPE (shadow_ptr_type);

  gimple_stmt_iterator gsi = *iter;

  if (!is_non_zero_len)
    {
      /* So, the length of the memory area to asan-protect is
	 non-constant.  Let's guard the generated instrumentation code
	 like:

	 if (len != 0)
	   {
	     //asan instrumentation code goes here.
	   }
	 // falltrough instructions, starting with *ITER.  */

      g = gimple_build_cond (NE_EXPR,
			    len,
			    build_int_cst (TREE_TYPE (len), 0),
			    NULL_TREE, NULL_TREE);
      gimple_set_location (g, loc);

      basic_block then_bb, fallthrough_bb;
      insert_if_then_before_iter (as_a <gcond *> (g), iter,
				  /*then_more_likely_p=*/true,
				  &then_bb, &fallthrough_bb);
      /* Note that fallthrough_bb starts with the statement that was
	pointed to by ITER.  */

      /* The 'then block' of the 'if (len != 0) condition is where
	we'll generate the asan instrumentation code now.  */
      gsi = gsi_last_bb (then_bb);
    }

  /* Get an iterator on the point where we can add the condition
     statement for the instrumentation.  */
  basic_block then_bb, else_bb;
  gsi = create_cond_insert_point (&gsi, /*before_p*/false,
				  /*then_more_likely_p=*/false,
				  /*create_then_fallthru_edge*/recover_p,
				  &then_bb,
				  &else_bb);

  g = gimple_build_assign (make_ssa_name (pointer_sized_int_node),
			   NOP_EXPR, base);
  gimple_set_location (g, loc);
  gsi_insert_before (&gsi, g, GSI_NEW_STMT);
  tree base_addr = gimple_assign_lhs (g);

  tree t = NULL_TREE;
  if (real_size_in_bytes >= 8)
    {
      tree shadow = build_shadow_mem_access (&gsi, loc, base_addr,
					     shadow_ptr_type);
      t = shadow;
    }
  else
    {
      /* Slow path for 1, 2 and 4 byte accesses.  */
      /* Test (shadow != 0)
	 & ((base_addr & 7) + (real_size_in_bytes - 1)) >= shadow).  */
      tree shadow = build_shadow_mem_access (&gsi, loc, base_addr,
					     shadow_ptr_type);
      gimple *shadow_test = build_assign (NE_EXPR, shadow, 0);
      gimple_seq seq = NULL;
      gimple_seq_add_stmt (&seq, shadow_test);
      /* Aligned (>= 8 bytes) can test just
	 (real_size_in_bytes - 1 >= shadow), as base_addr & 7 is known
	 to be 0.  */
      if (align < 8)
	{
	  gimple_seq_add_stmt (&seq, build_assign (BIT_AND_EXPR,
						   base_addr, 7));
	  gimple_seq_add_stmt (&seq,
			       build_type_cast (shadow_type,
						gimple_seq_last (seq)));
	  if (real_size_in_bytes > 1)
	    gimple_seq_add_stmt (&seq,
				 build_assign (PLUS_EXPR,
					       gimple_seq_last (seq),
					       real_size_in_bytes - 1));
	  t = gimple_assign_lhs (gimple_seq_last_stmt (seq));
	}
      else
	t = build_int_cst (shadow_type, real_size_in_bytes - 1);
      gimple_seq_add_stmt (&seq, build_assign (GE_EXPR, t, shadow));
      gimple_seq_add_stmt (&seq, build_assign (BIT_AND_EXPR, shadow_test,
					       gimple_seq_last (seq)));
      t = gimple_assign_lhs (gimple_seq_last (seq));
      gimple_seq_set_location (seq, loc);
      gsi_insert_seq_after (&gsi, seq, GSI_CONTINUE_LINKING);

      /* For non-constant, misaligned or otherwise weird access sizes,
       check first and last byte.  */
      if (size_in_bytes == -1)
	{
	  g = gimple_build_assign (make_ssa_name (pointer_sized_int_node),
				   MINUS_EXPR, len,
				   build_int_cst (pointer_sized_int_node, 1));
	  gimple_set_location (g, loc);
	  gsi_insert_after (&gsi, g, GSI_NEW_STMT);
	  tree last = gimple_assign_lhs (g);
	  g = gimple_build_assign (make_ssa_name (pointer_sized_int_node),
				   PLUS_EXPR, base_addr, last);
	  gimple_set_location (g, loc);
	  gsi_insert_after (&gsi, g, GSI_NEW_STMT);
	  tree base_end_addr = gimple_assign_lhs (g);

	  tree shadow = build_shadow_mem_access (&gsi, loc, base_end_addr,
						 shadow_ptr_type);
	  gimple *shadow_test = build_assign (NE_EXPR, shadow, 0);
	  gimple_seq seq = NULL;
	  gimple_seq_add_stmt (&seq, shadow_test);
	  gimple_seq_add_stmt (&seq, build_assign (BIT_AND_EXPR,
						   base_end_addr, 7));
	  gimple_seq_add_stmt (&seq, build_type_cast (shadow_type,
						      gimple_seq_last (seq)));
	  gimple_seq_add_stmt (&seq, build_assign (GE_EXPR,
						   gimple_seq_last (seq),
						   shadow));
	  gimple_seq_add_stmt (&seq, build_assign (BIT_AND_EXPR, shadow_test,
						   gimple_seq_last (seq)));
	  gimple_seq_add_stmt (&seq, build_assign (BIT_IOR_EXPR, t,
						   gimple_seq_last (seq)));
	  t = gimple_assign_lhs (gimple_seq_last (seq));
	  gimple_seq_set_location (seq, loc);
	  gsi_insert_seq_after (&gsi, seq, GSI_CONTINUE_LINKING);
	}
    }

  g = gimple_build_cond (NE_EXPR, t, build_int_cst (TREE_TYPE (t), 0),
			 NULL_TREE, NULL_TREE);
  gimple_set_location (g, loc);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);

  /* Generate call to the run-time library (e.g. __asan_report_load8).  */
  gsi = gsi_start_bb (then_bb);
  int nargs;
  tree fun = report_error_func (is_store, recover_p, size_in_bytes, &nargs);
  g = gimple_build_call (fun, nargs, base_addr, len);
  gimple_set_location (g, loc);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);

  gsi_remove (iter, true);
  *iter = gsi_start_bb (else_bb);

  return true;
}

/* Create ASAN shadow variable for a VAR_DECL which has been rewritten
   into SSA.  Already seen VAR_DECLs are stored in SHADOW_VARS_MAPPING.  */

static tree
create_asan_shadow_var (tree var_decl,
			hash_map<tree, tree> &shadow_vars_mapping)
{
  tree *slot = shadow_vars_mapping.get (var_decl);
  if (slot == NULL)
    {
      tree shadow_var = copy_node (var_decl);

      copy_body_data id;
      memset (&id, 0, sizeof (copy_body_data));
      id.src_fn = id.dst_fn = current_function_decl;
      copy_decl_for_dup_finish (&id, var_decl, shadow_var);

      DECL_ARTIFICIAL (shadow_var) = 1;
      DECL_IGNORED_P (shadow_var) = 1;
      DECL_SEEN_IN_BIND_EXPR_P (shadow_var) = 0;
      gimple_add_tmp_var (shadow_var);

      shadow_vars_mapping.put (var_decl, shadow_var);
      return shadow_var;
    }
  else
    return *slot;
}

/* Expand ASAN_POISON ifn.  */

bool
asan_expand_poison_ifn (gimple_stmt_iterator *iter,
			bool *need_commit_edge_insert,
			hash_map<tree, tree> &shadow_vars_mapping)
{
  gimple *g = gsi_stmt (*iter);
  tree poisoned_var = gimple_call_lhs (g);
  if (!poisoned_var || has_zero_uses (poisoned_var))
    {
      gsi_remove (iter, true);
      return true;
    }

  if (SSA_NAME_VAR (poisoned_var) == NULL_TREE)
    SET_SSA_NAME_VAR_OR_IDENTIFIER (poisoned_var,
				    create_tmp_var (TREE_TYPE (poisoned_var)));

  tree shadow_var = create_asan_shadow_var (SSA_NAME_VAR (poisoned_var),
					    shadow_vars_mapping);

  bool recover_p;
  if (flag_sanitize & SANITIZE_USER_ADDRESS)
    recover_p = (flag_sanitize_recover & SANITIZE_USER_ADDRESS) != 0;
  else
    recover_p = (flag_sanitize_recover & SANITIZE_KERNEL_ADDRESS) != 0;
  tree size = DECL_SIZE_UNIT (shadow_var);
  gimple *poison_call
    = gimple_build_call_internal (IFN_ASAN_MARK, 3,
				  build_int_cst (integer_type_node,
						 ASAN_MARK_POISON),
				  build_fold_addr_expr (shadow_var), size);

  gimple *use;
  imm_use_iterator imm_iter;
  FOR_EACH_IMM_USE_STMT (use, imm_iter, poisoned_var)
    {
      if (is_gimple_debug (use))
	continue;

      int nargs;
      bool store_p = gimple_call_internal_p (use, IFN_ASAN_POISON_USE);
      tree fun = report_error_func (store_p, recover_p, tree_to_uhwi (size),
				    &nargs);

      gcall *call = gimple_build_call (fun, 1,
				       build_fold_addr_expr (shadow_var));
      gimple_set_location (call, gimple_location (use));
      gimple *call_to_insert = call;

      /* The USE can be a gimple PHI node.  If so, insert the call on
	 all edges leading to the PHI node.  */
      if (is_a <gphi *> (use))
	{
	  gphi *phi = dyn_cast<gphi *> (use);
	  for (unsigned i = 0; i < gimple_phi_num_args (phi); ++i)
	    if (gimple_phi_arg_def (phi, i) == poisoned_var)
	      {
		edge e = gimple_phi_arg_edge (phi, i);

		if (call_to_insert == NULL)
		  call_to_insert = gimple_copy (call);

		gsi_insert_seq_on_edge (e, call_to_insert);
		*need_commit_edge_insert = true;
		call_to_insert = NULL;
	      }
	}
      else
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (use);
	  if (store_p)
	    gsi_replace (&gsi, call, true);
	  else
	    gsi_insert_before (&gsi, call, GSI_NEW_STMT);
	}
    }

  SSA_NAME_IS_DEFAULT_DEF (poisoned_var) = true;
  SSA_NAME_DEF_STMT (poisoned_var) = gimple_build_nop ();
  gsi_replace (iter, poison_call, false);

  return true;
}

/* Instrument the current function.  */

static unsigned int
asan_instrument (void)
{
  if (shadow_ptr_types[0] == NULL_TREE)
    asan_init_shadow_ptr_types ();
  transform_statements ();
  last_alloca_addr = NULL_TREE;
  return 0;
}

static bool
gate_asan (void)
{
  return sanitize_flags_p (SANITIZE_ADDRESS);
}

namespace {

const pass_data pass_data_asan =
{
  GIMPLE_PASS, /* type */
  "asan", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_ssa | PROP_cfg | PROP_gimple_leh ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_asan : public gimple_opt_pass
{
public:
  pass_asan (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_asan, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_asan (m_ctxt); }
  virtual bool gate (function *) { return gate_asan (); }
  virtual unsigned int execute (function *) { return asan_instrument (); }

}; // class pass_asan

} // anon namespace

gimple_opt_pass *
make_pass_asan (gcc::context *ctxt)
{
  return new pass_asan (ctxt);
}

namespace {

const pass_data pass_data_asan_O0 =
{
  GIMPLE_PASS, /* type */
  "asan0", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_ssa | PROP_cfg | PROP_gimple_leh ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_asan_O0 : public gimple_opt_pass
{
public:
  pass_asan_O0 (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_asan_O0, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return !optimize && gate_asan (); }
  virtual unsigned int execute (function *) { return asan_instrument (); }

}; // class pass_asan_O0

} // anon namespace

gimple_opt_pass *
make_pass_asan_O0 (gcc::context *ctxt)
{
  return new pass_asan_O0 (ctxt);
}

#include "gt-asan.h"
