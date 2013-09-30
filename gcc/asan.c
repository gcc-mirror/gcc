/* AddressSanitizer, a fast memory error detector.
   Copyright (C) 2012-2013 Free Software Foundation, Inc.
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
#include "gimple.h"
#include "tree-iterator.h"
#include "tree-ssa.h"
#include "tree-pass.h"
#include "asan.h"
#include "gimple-pretty-print.h"
#include "target.h"
#include "expr.h"
#include "optabs.h"
#include "output.h"
#include "tm_p.h"
#include "langhooks.h"
#include "hash-table.h"
#include "alloc-pool.h"
#include "cfgloop.h"
#include "gimple-builder.h"

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
   A call too __asan_init() is inserted to the list of module CTORs.

   The run-time library redefines malloc (so that redzone are inserted around
   the allocated memory) and free (so that reuse of free-ed memory is delayed),
   provides __asan_report* and __asan_init functions.

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

      3/ The following 16 bytes of the red zone have no particular
      format.

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

       // This is always set to NULL for now.
       uptr __has_dynamic_init;
     }

   A destructor function that calls the runtime asan library function
   _asan_unregister_globals is also installed.  */

alias_set_type asan_shadow_set = -1;

/* Pointer types to 1 resp. 2 byte integers in shadow memory.  A separate
   alias set is used for all shadow memory accesses.  */
static GTY(()) tree shadow_ptr_types[2];

/* Hashtable support for memory references used by gimple
   statements.  */

/* This type represents a reference to a memory region.  */
struct asan_mem_ref
{
  /* The expression of the beginning of the memory region.  */
  tree start;

  /* The size of the access (can be 1, 2, 4, 8, 16 for now).  */
  char access_size;
};

static alloc_pool asan_mem_ref_alloc_pool;

/* This creates the alloc pool used to store the instances of
   asan_mem_ref that are stored in the hash table asan_mem_ref_ht.  */

static alloc_pool
asan_mem_ref_get_alloc_pool ()
{
  if (asan_mem_ref_alloc_pool == NULL)
    asan_mem_ref_alloc_pool = create_alloc_pool ("asan_mem_ref",
						 sizeof (asan_mem_ref),
						 10);
  return asan_mem_ref_alloc_pool;
    
}

/* Initializes an instance of asan_mem_ref.  */

static void
asan_mem_ref_init (asan_mem_ref *ref, tree start, char access_size)
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
asan_mem_ref_new (tree start, char access_size)
{
  asan_mem_ref *ref =
    (asan_mem_ref *) pool_alloc (asan_mem_ref_get_alloc_pool ());

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

struct asan_mem_ref_hasher
  : typed_noop_remove <asan_mem_ref>
{
  typedef asan_mem_ref value_type;
  typedef asan_mem_ref compare_type;

  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
};

/* Hash a memory reference.  */

inline hashval_t
asan_mem_ref_hasher::hash (const asan_mem_ref *mem_ref)
{
  hashval_t h = iterative_hash_expr (mem_ref->start, 0);
  h = iterative_hash_hashval_t (h, mem_ref->access_size);
  return h;
}

/* Compare two memory references.  We accept the length of either
   memory references to be NULL_TREE.  */

inline bool
asan_mem_ref_hasher::equal (const asan_mem_ref *m1,
			    const asan_mem_ref *m2)
{
  return (m1->access_size == m2->access_size
	  && operand_equal_p (m1->start, m2->start, 0));
}

static hash_table <asan_mem_ref_hasher> asan_mem_ref_ht;

/* Returns a reference to the hash table containing memory references.
   This function ensures that the hash table is created.  Note that
   this hash table is updated by the function
   update_mem_ref_hash_table.  */

static hash_table <asan_mem_ref_hasher> &
get_mem_ref_hash_table ()
{
  if (!asan_mem_ref_ht.is_created ())
    asan_mem_ref_ht.create (10);

  return asan_mem_ref_ht;
}

/* Clear all entries from the memory references hash table.  */

static void
empty_mem_ref_hash_table ()
{
  if (asan_mem_ref_ht.is_created ())
    asan_mem_ref_ht.empty ();
}

/* Free the memory references hash table.  */

static void
free_mem_ref_resources ()
{
  if (asan_mem_ref_ht.is_created ())
    asan_mem_ref_ht.dispose ();

  if (asan_mem_ref_alloc_pool)
    {
      free_alloc_pool (asan_mem_ref_alloc_pool);
      asan_mem_ref_alloc_pool = NULL;
    }
}

/* Return true iff the memory reference REF has been instrumented.  */

static bool
has_mem_ref_been_instrumented (tree ref, char access_size)
{
  asan_mem_ref r;
  asan_mem_ref_init (&r, ref, access_size);

  return (get_mem_ref_hash_table ().find (&r) != NULL);
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
  /* First let's see if the address of the beginning of REF has been
     instrumented.  */
  if (!has_mem_ref_been_instrumented (ref))
    return false;

  if (len != 0)
    {
      /* Let's see if the end of the region has been instrumented.  */
      if (!has_mem_ref_been_instrumented (asan_mem_ref_get_end (ref, len),
					  ref->access_size))
	return false;
    }
  return true;
}

/* Set REF to the memory reference present in a gimple assignment
   ASSIGNMENT.  Return true upon successful completion, false
   otherwise.  */

static bool
get_mem_ref_of_assignment (const gimple assignment,
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

/* Return the memory references contained in a gimple statement
   representing a builtin call that has to do with memory access.  */

static bool
get_mem_refs_of_builtin_call (const gimple call,
			      asan_mem_ref *src0,
			      tree *src0_len,
			      bool *src0_is_store,
			      asan_mem_ref *src1,
			      tree *src1_len,
			      bool *src1_is_store,
			      asan_mem_ref *dst,
			      tree *dst_len,
			      bool *dst_is_store,
			      bool *dest_is_deref)
{
  gcc_checking_assert (gimple_call_builtin_p (call, BUILT_IN_NORMAL));

  tree callee = gimple_call_fndecl (call);
  tree source0 = NULL_TREE, source1 = NULL_TREE,
    dest = NULL_TREE, len = NULL_TREE;
  bool is_store = true, got_reference_p = false;
  char access_size = 1;

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
      break ;

    /* And now the __atomic* and __sync builtins.
       These are handled differently from the classical memory memory
       access builtins above.  */

    case BUILT_IN_ATOMIC_LOAD_1:
    case BUILT_IN_ATOMIC_LOAD_2:
    case BUILT_IN_ATOMIC_LOAD_4:
    case BUILT_IN_ATOMIC_LOAD_8:
    case BUILT_IN_ATOMIC_LOAD_16:
      is_store = false;
      /* fall through.  */

    case BUILT_IN_SYNC_FETCH_AND_ADD_1:
    case BUILT_IN_SYNC_FETCH_AND_ADD_2:
    case BUILT_IN_SYNC_FETCH_AND_ADD_4:
    case BUILT_IN_SYNC_FETCH_AND_ADD_8:
    case BUILT_IN_SYNC_FETCH_AND_ADD_16:

    case BUILT_IN_SYNC_FETCH_AND_SUB_1:
    case BUILT_IN_SYNC_FETCH_AND_SUB_2:
    case BUILT_IN_SYNC_FETCH_AND_SUB_4:
    case BUILT_IN_SYNC_FETCH_AND_SUB_8:
    case BUILT_IN_SYNC_FETCH_AND_SUB_16:

    case BUILT_IN_SYNC_FETCH_AND_OR_1:
    case BUILT_IN_SYNC_FETCH_AND_OR_2:
    case BUILT_IN_SYNC_FETCH_AND_OR_4:
    case BUILT_IN_SYNC_FETCH_AND_OR_8:
    case BUILT_IN_SYNC_FETCH_AND_OR_16:

    case BUILT_IN_SYNC_FETCH_AND_AND_1:
    case BUILT_IN_SYNC_FETCH_AND_AND_2:
    case BUILT_IN_SYNC_FETCH_AND_AND_4:
    case BUILT_IN_SYNC_FETCH_AND_AND_8:
    case BUILT_IN_SYNC_FETCH_AND_AND_16:

    case BUILT_IN_SYNC_FETCH_AND_XOR_1:
    case BUILT_IN_SYNC_FETCH_AND_XOR_2:
    case BUILT_IN_SYNC_FETCH_AND_XOR_4:
    case BUILT_IN_SYNC_FETCH_AND_XOR_8:
    case BUILT_IN_SYNC_FETCH_AND_XOR_16:

    case BUILT_IN_SYNC_FETCH_AND_NAND_1:
    case BUILT_IN_SYNC_FETCH_AND_NAND_2:
    case BUILT_IN_SYNC_FETCH_AND_NAND_4:
    case BUILT_IN_SYNC_FETCH_AND_NAND_8:

    case BUILT_IN_SYNC_ADD_AND_FETCH_1:
    case BUILT_IN_SYNC_ADD_AND_FETCH_2:
    case BUILT_IN_SYNC_ADD_AND_FETCH_4:
    case BUILT_IN_SYNC_ADD_AND_FETCH_8:
    case BUILT_IN_SYNC_ADD_AND_FETCH_16:

    case BUILT_IN_SYNC_SUB_AND_FETCH_1:
    case BUILT_IN_SYNC_SUB_AND_FETCH_2:
    case BUILT_IN_SYNC_SUB_AND_FETCH_4:
    case BUILT_IN_SYNC_SUB_AND_FETCH_8:
    case BUILT_IN_SYNC_SUB_AND_FETCH_16:

    case BUILT_IN_SYNC_OR_AND_FETCH_1:
    case BUILT_IN_SYNC_OR_AND_FETCH_2:
    case BUILT_IN_SYNC_OR_AND_FETCH_4:
    case BUILT_IN_SYNC_OR_AND_FETCH_8:
    case BUILT_IN_SYNC_OR_AND_FETCH_16:

    case BUILT_IN_SYNC_AND_AND_FETCH_1:
    case BUILT_IN_SYNC_AND_AND_FETCH_2:
    case BUILT_IN_SYNC_AND_AND_FETCH_4:
    case BUILT_IN_SYNC_AND_AND_FETCH_8:
    case BUILT_IN_SYNC_AND_AND_FETCH_16:

    case BUILT_IN_SYNC_XOR_AND_FETCH_1:
    case BUILT_IN_SYNC_XOR_AND_FETCH_2:
    case BUILT_IN_SYNC_XOR_AND_FETCH_4:
    case BUILT_IN_SYNC_XOR_AND_FETCH_8:
    case BUILT_IN_SYNC_XOR_AND_FETCH_16:

    case BUILT_IN_SYNC_NAND_AND_FETCH_1:
    case BUILT_IN_SYNC_NAND_AND_FETCH_2:
    case BUILT_IN_SYNC_NAND_AND_FETCH_4:
    case BUILT_IN_SYNC_NAND_AND_FETCH_8:

    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_1:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_2:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_4:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_8:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_16:

    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_1:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_2:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_4:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_8:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_16:

    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_1:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_2:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_4:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_8:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_16:

    case BUILT_IN_SYNC_LOCK_RELEASE_1:
    case BUILT_IN_SYNC_LOCK_RELEASE_2:
    case BUILT_IN_SYNC_LOCK_RELEASE_4:
    case BUILT_IN_SYNC_LOCK_RELEASE_8:
    case BUILT_IN_SYNC_LOCK_RELEASE_16:

    case BUILT_IN_ATOMIC_EXCHANGE_1:
    case BUILT_IN_ATOMIC_EXCHANGE_2:
    case BUILT_IN_ATOMIC_EXCHANGE_4:
    case BUILT_IN_ATOMIC_EXCHANGE_8:
    case BUILT_IN_ATOMIC_EXCHANGE_16:

    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_1:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_2:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_4:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_8:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_16:

    case BUILT_IN_ATOMIC_STORE_1:
    case BUILT_IN_ATOMIC_STORE_2:
    case BUILT_IN_ATOMIC_STORE_4:
    case BUILT_IN_ATOMIC_STORE_8:
    case BUILT_IN_ATOMIC_STORE_16:

    case BUILT_IN_ATOMIC_ADD_FETCH_1:
    case BUILT_IN_ATOMIC_ADD_FETCH_2:
    case BUILT_IN_ATOMIC_ADD_FETCH_4:
    case BUILT_IN_ATOMIC_ADD_FETCH_8:
    case BUILT_IN_ATOMIC_ADD_FETCH_16:

    case BUILT_IN_ATOMIC_SUB_FETCH_1:
    case BUILT_IN_ATOMIC_SUB_FETCH_2:
    case BUILT_IN_ATOMIC_SUB_FETCH_4:
    case BUILT_IN_ATOMIC_SUB_FETCH_8:
    case BUILT_IN_ATOMIC_SUB_FETCH_16:

    case BUILT_IN_ATOMIC_AND_FETCH_1:
    case BUILT_IN_ATOMIC_AND_FETCH_2:
    case BUILT_IN_ATOMIC_AND_FETCH_4:
    case BUILT_IN_ATOMIC_AND_FETCH_8:
    case BUILT_IN_ATOMIC_AND_FETCH_16:

    case BUILT_IN_ATOMIC_NAND_FETCH_1:
    case BUILT_IN_ATOMIC_NAND_FETCH_2:
    case BUILT_IN_ATOMIC_NAND_FETCH_4:
    case BUILT_IN_ATOMIC_NAND_FETCH_8:
    case BUILT_IN_ATOMIC_NAND_FETCH_16:

    case BUILT_IN_ATOMIC_XOR_FETCH_1:
    case BUILT_IN_ATOMIC_XOR_FETCH_2:
    case BUILT_IN_ATOMIC_XOR_FETCH_4:
    case BUILT_IN_ATOMIC_XOR_FETCH_8:
    case BUILT_IN_ATOMIC_XOR_FETCH_16:

    case BUILT_IN_ATOMIC_OR_FETCH_1:
    case BUILT_IN_ATOMIC_OR_FETCH_2:
    case BUILT_IN_ATOMIC_OR_FETCH_4:
    case BUILT_IN_ATOMIC_OR_FETCH_8:
    case BUILT_IN_ATOMIC_OR_FETCH_16:

    case BUILT_IN_ATOMIC_FETCH_ADD_1:
    case BUILT_IN_ATOMIC_FETCH_ADD_2:
    case BUILT_IN_ATOMIC_FETCH_ADD_4:
    case BUILT_IN_ATOMIC_FETCH_ADD_8:
    case BUILT_IN_ATOMIC_FETCH_ADD_16:

    case BUILT_IN_ATOMIC_FETCH_SUB_1:
    case BUILT_IN_ATOMIC_FETCH_SUB_2:
    case BUILT_IN_ATOMIC_FETCH_SUB_4:
    case BUILT_IN_ATOMIC_FETCH_SUB_8:
    case BUILT_IN_ATOMIC_FETCH_SUB_16:

    case BUILT_IN_ATOMIC_FETCH_AND_1:
    case BUILT_IN_ATOMIC_FETCH_AND_2:
    case BUILT_IN_ATOMIC_FETCH_AND_4:
    case BUILT_IN_ATOMIC_FETCH_AND_8:
    case BUILT_IN_ATOMIC_FETCH_AND_16:

    case BUILT_IN_ATOMIC_FETCH_NAND_1:
    case BUILT_IN_ATOMIC_FETCH_NAND_2:
    case BUILT_IN_ATOMIC_FETCH_NAND_4:
    case BUILT_IN_ATOMIC_FETCH_NAND_8:
    case BUILT_IN_ATOMIC_FETCH_NAND_16:

    case BUILT_IN_ATOMIC_FETCH_XOR_1:
    case BUILT_IN_ATOMIC_FETCH_XOR_2:
    case BUILT_IN_ATOMIC_FETCH_XOR_4:
    case BUILT_IN_ATOMIC_FETCH_XOR_8:
    case BUILT_IN_ATOMIC_FETCH_XOR_16:

    case BUILT_IN_ATOMIC_FETCH_OR_1:
    case BUILT_IN_ATOMIC_FETCH_OR_2:
    case BUILT_IN_ATOMIC_FETCH_OR_4:
    case BUILT_IN_ATOMIC_FETCH_OR_8:
    case BUILT_IN_ATOMIC_FETCH_OR_16:
      {
	dest = gimple_call_arg (call, 0);
	/* DEST represents the address of a memory location.
	   instrument_derefs wants the memory location, so lets
	   dereference the address DEST before handing it to
	   instrument_derefs.  */
	if (TREE_CODE (dest) == ADDR_EXPR)
	  dest = TREE_OPERAND (dest, 0);
	else if (TREE_CODE (dest) == SSA_NAME)
	  dest = build2 (MEM_REF, TREE_TYPE (TREE_TYPE (dest)),
			 dest, build_int_cst (TREE_TYPE (dest), 0));
	else
	  gcc_unreachable ();

	access_size = int_size_in_bytes (TREE_TYPE (dest));
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
has_stmt_been_instrumented_p (gimple stmt)
{
  if (gimple_assign_single_p (stmt))
    {
      bool r_is_store;
      asan_mem_ref r;
      asan_mem_ref_init (&r, NULL, 1);

      if (get_mem_ref_of_assignment (stmt, &r, &r_is_store))
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
	dest_is_store = false, dest_is_deref = false;
      if (get_mem_refs_of_builtin_call (stmt,
					&src0, &src0_len, &src0_is_store,
					&src1, &src1_len, &src1_is_store,
					&dest, &dest_len, &dest_is_store,
					&dest_is_deref))
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
  return false;
}

/*  Insert a memory reference into the hash table.  */

static void
update_mem_ref_hash_table (tree ref, char access_size)
{
  hash_table <asan_mem_ref_hasher> ht = get_mem_ref_hash_table ();

  asan_mem_ref r;
  asan_mem_ref_init (&r, ref, access_size);

  asan_mem_ref **slot = ht.find_slot (&r, INSERT);
  if (*slot == NULL)
    *slot = asan_mem_ref_new (ref, access_size);
}

/* Initialize shadow_ptr_types array.  */

static void
asan_init_shadow_ptr_types (void)
{
  asan_shadow_set = new_alias_set ();
  shadow_ptr_types[0] = build_distinct_type_copy (signed_char_type_node);
  TYPE_ALIAS_SET (shadow_ptr_types[0]) = asan_shadow_set;
  shadow_ptr_types[0] = build_pointer_type (shadow_ptr_types[0]);
  shadow_ptr_types[1] = build_distinct_type_copy (short_integer_type_node);
  TYPE_ALIAS_SET (shadow_ptr_types[1]) = asan_shadow_set;
  shadow_ptr_types[1] = build_pointer_type (shadow_ptr_types[1]);
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
  rtx insn, insns, top_label, end, addr, tmp, jump;

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
  addr = force_reg (Pmode, XEXP (shadow_mem, 0));
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
  add_int_reg_note (jump, REG_BR_PROB, REG_BR_PROB_BASE * 80 / 100);
}

/* Insert code to protect stack vars.  The prologue sequence should be emitted
   directly, epilogue sequence returned.  BASE is the register holding the
   stack base, against which OFFSETS array offsets are relative to, OFFSETS
   array contains pairs of offsets in reverse order, always the end offset
   of some gap that needs protection followed by starting offset,
   and DECLS is an array of representative decls for each var partition.
   LENGTH is the length of the OFFSETS array, DECLS array is LENGTH / 2 - 1
   elements long (OFFSETS include gap before the first variable as well
   as gaps after each stack variable).  */

rtx
asan_emit_stack_protection (rtx base, HOST_WIDE_INT *offsets, tree *decls,
			    int length)
{
  rtx shadow_base, shadow_mem, ret, mem;
  unsigned char shadow_bytes[4];
  HOST_WIDE_INT base_offset = offsets[length - 1], offset, prev_offset;
  HOST_WIDE_INT last_offset, last_size;
  int l;
  unsigned char cur_shadow_byte = ASAN_STACK_MAGIC_LEFT;
  tree str_cst;

  if (shadow_ptr_types[0] == NULL_TREE)
    asan_init_shadow_ptr_types ();

  /* First of all, prepare the description string.  */
  pretty_printer asan_pp;

  if (DECL_NAME (current_function_decl))
    pp_tree_identifier (&asan_pp, DECL_NAME (current_function_decl));
  else
    pp_string (&asan_pp, "<unknown>");
  pp_space (&asan_pp);
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
  base = expand_binop (Pmode, add_optab, base,
		       gen_int_mode (base_offset, Pmode),
		       NULL_RTX, 1, OPTAB_DIRECT);
  mem = gen_rtx_MEM (ptr_mode, base);
  emit_move_insn (mem, gen_int_mode (ASAN_STACK_FRAME_MAGIC, ptr_mode));
  mem = adjust_address (mem, VOIDmode, GET_MODE_SIZE (ptr_mode));
  emit_move_insn (mem, expand_normal (str_cst));
  shadow_base = expand_binop (Pmode, lshr_optab, base,
			      GEN_INT (ASAN_SHADOW_SHIFT),
			      NULL_RTX, 1, OPTAB_DIRECT);
  shadow_base = expand_binop (Pmode, add_optab, shadow_base,
			      gen_int_mode (targetm.asan_shadow_offset (),
					    Pmode),
			      NULL_RTX, 1, OPTAB_DIRECT);
  gcc_assert (asan_shadow_set != -1
	      && (ASAN_RED_ZONE_SIZE >> ASAN_SHADOW_SHIFT) == 4);
  shadow_mem = gen_rtx_MEM (SImode, shadow_base);
  set_mem_alias_set (shadow_mem, asan_shadow_set);
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
	  for (i = 0; i < 4; i++, aoff += (1 << ASAN_SHADOW_SHIFT))
	    if (aoff < offset)
	      {
		if (aoff < offset - (1 << ASAN_SHADOW_SHIFT) + 1)
		  shadow_bytes[i] = 0;
		else
		  shadow_bytes[i] = offset - aoff;
	      }
	    else
	      shadow_bytes[i] = ASAN_STACK_MAGIC_PARTIAL;
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

  shadow_mem = gen_rtx_MEM (BLKmode, shadow_base);
  set_mem_alias_set (shadow_mem, asan_shadow_set);
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
    }
  if (last_size)
    {
      shadow_mem = adjust_address (shadow_mem, VOIDmode,
				   (last_offset - prev_offset)
				   >> ASAN_SHADOW_SHIFT);
      asan_clear_shadow (shadow_mem, last_size >> ASAN_SHADOW_SHIFT);
    }

  do_pending_stack_adjust ();

  ret = get_insns ();
  end_sequence ();
  return ret;
}

/* Return true if DECL, a global var, might be overridden and needs
   therefore a local alias.  */

static bool
asan_needs_local_alias (tree decl)
{
  return DECL_WEAK (decl) || !targetm.binds_local_p (decl);
}

/* Return true if DECL is a VAR_DECL that should be protected
   by Address Sanitizer, by appending a red zone with protected
   shadow memory after it and aligning it to at least
   ASAN_RED_ZONE_SIZE bytes.  */

bool
asan_protect_global (tree decl)
{
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
  if (TREE_CODE (decl) != VAR_DECL
      /* TLS vars aren't statically protectable.  */
      || DECL_THREAD_LOCAL_P (decl)
      /* Externs will be protected elsewhere.  */
      || DECL_EXTERNAL (decl)
      || !DECL_RTL_SET_P (decl)
      /* Comdat vars pose an ABI problem, we can't know if
	 the var that is selected by the linker will have
	 padding or not.  */
      || DECL_ONE_ONLY (decl)
      /* Similarly for common vars.  People can use -fno-common.  */
      || (DECL_COMMON (decl) && TREE_PUBLIC (decl))
      /* Don't protect if using user section, often vars placed
	 into user section from multiple TUs are then assumed
	 to be an array of such vars, putting padding in there
	 breaks this assumption.  */
      || (DECL_SECTION_NAME (decl) != NULL_TREE
	  && !DECL_HAS_IMPLICIT_SECTION_NAME_P (decl))
      || DECL_SIZE (decl) == 0
      || ASAN_RED_ZONE_SIZE * BITS_PER_UNIT > MAX_OFILE_ALIGNMENT
      || !valid_constant_size_p (DECL_SIZE_UNIT (decl))
      || DECL_ALIGN_UNIT (decl) > 2 * ASAN_RED_ZONE_SIZE)
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

#ifndef ASM_OUTPUT_DEF
  if (asan_needs_local_alias (decl))
    return false;
#endif

  return true;
}

/* Construct a function tree for __asan_report_{load,store}{1,2,4,8,16}.
   IS_STORE is either 1 (for a store) or 0 (for a load).
   SIZE_IN_BYTES is one of 1, 2, 4, 8, 16.  */

static tree
report_error_func (bool is_store, int size_in_bytes)
{
  static enum built_in_function report[2][5]
    = { { BUILT_IN_ASAN_REPORT_LOAD1, BUILT_IN_ASAN_REPORT_LOAD2,
	  BUILT_IN_ASAN_REPORT_LOAD4, BUILT_IN_ASAN_REPORT_LOAD8,
	  BUILT_IN_ASAN_REPORT_LOAD16 },
	{ BUILT_IN_ASAN_REPORT_STORE1, BUILT_IN_ASAN_REPORT_STORE2,
	  BUILT_IN_ASAN_REPORT_STORE4, BUILT_IN_ASAN_REPORT_STORE8,
	  BUILT_IN_ASAN_REPORT_STORE16 } };
  return builtin_decl_implicit (report[is_store][exact_log2 (size_in_bytes)]);
}

#define PROB_VERY_UNLIKELY	(REG_BR_PROB_BASE / 2000 - 1)
#define PROB_ALWAYS		(REG_BR_PROB_BASE)

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

static gimple_stmt_iterator
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
  int fallthrough_probability
    = then_more_likely_p
    ? PROB_VERY_UNLIKELY
    : PROB_ALWAYS - PROB_VERY_UNLIKELY;
  e->probability = PROB_ALWAYS - fallthrough_probability;
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
insert_if_then_before_iter (gimple cond,
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

/* Instrument the memory access instruction BASE.  Insert new
   statements before or after ITER.

   Note that the memory access represented by BASE can be either an
   SSA_NAME, or a non-SSA expression.  LOCATION is the source code
   location.  IS_STORE is TRUE for a store, FALSE for a load.
   BEFORE_P is TRUE for inserting the instrumentation code before
   ITER, FALSE for inserting it after ITER.  SIZE_IN_BYTES is one of
   1, 2, 4, 8, 16.

   If BEFORE_P is TRUE, *ITER is arranged to still point to the
   statement it was pointing to prior to calling this function,
   otherwise, it points to the statement logically following it.  */

static void
build_check_stmt (location_t location, tree base, gimple_stmt_iterator *iter,
		  bool before_p, bool is_store, int size_in_bytes)
{
  gimple_stmt_iterator gsi;
  basic_block then_bb, else_bb;
  tree t, base_addr, shadow;
  gimple g;
  tree shadow_ptr_type = shadow_ptr_types[size_in_bytes == 16 ? 1 : 0];
  tree shadow_type = TREE_TYPE (shadow_ptr_type);
  tree uintptr_type
    = build_nonstandard_integer_type (TYPE_PRECISION (TREE_TYPE (base)), 1);
  tree base_ssa = base;

  /* Get an iterator on the point where we can add the condition
     statement for the instrumentation.  */
  gsi = create_cond_insert_point (iter, before_p,
				  /*then_more_likely_p=*/false,
				  /*create_then_fallthru_edge=*/false,
				  &then_bb,
				  &else_bb);

  base = unshare_expr (base);

  /* BASE can already be an SSA_NAME; in that case, do not create a
     new SSA_NAME for it.  */
  if (TREE_CODE (base) != SSA_NAME)
    {
      g = gimple_build_assign_with_ops (TREE_CODE (base),
					make_ssa_name (TREE_TYPE (base), NULL),
					base, NULL_TREE);
      gimple_set_location (g, location);
      gsi_insert_after (&gsi, g, GSI_NEW_STMT);
      base_ssa = gimple_assign_lhs (g);
    }

  g = gimple_build_assign_with_ops (NOP_EXPR,
				    make_ssa_name (uintptr_type, NULL),
				    base_ssa, NULL_TREE);
  gimple_set_location (g, location);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);
  base_addr = gimple_assign_lhs (g);

  /* Build
     (base_addr >> ASAN_SHADOW_SHIFT) + targetm.asan_shadow_offset ().  */

  t = build_int_cst (uintptr_type, ASAN_SHADOW_SHIFT);
  g = gimple_build_assign_with_ops (RSHIFT_EXPR,
				    make_ssa_name (uintptr_type, NULL),
				    base_addr, t);
  gimple_set_location (g, location);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);

  t = build_int_cst (uintptr_type, targetm.asan_shadow_offset ());
  g = gimple_build_assign_with_ops (PLUS_EXPR,
				    make_ssa_name (uintptr_type, NULL),
				    gimple_assign_lhs (g), t);
  gimple_set_location (g, location);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);

  g = gimple_build_assign_with_ops (NOP_EXPR,
				    make_ssa_name (shadow_ptr_type, NULL),
				    gimple_assign_lhs (g), NULL_TREE);
  gimple_set_location (g, location);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);

  t = build2 (MEM_REF, shadow_type, gimple_assign_lhs (g),
	      build_int_cst (shadow_ptr_type, 0));
  g = gimple_build_assign_with_ops (MEM_REF,
				    make_ssa_name (shadow_type, NULL),
				    t, NULL_TREE);
  gimple_set_location (g, location);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);
  shadow = gimple_assign_lhs (g);

  if (size_in_bytes < 8)
    {
      /* Slow path for 1, 2 and 4 byte accesses.
	 Test (shadow != 0)
	      & ((base_addr & 7) + (size_in_bytes - 1)) >= shadow).  */
      gimple_seq seq = NULL;
      gimple shadow_test = build_assign (NE_EXPR, shadow, 0);
      gimple_seq_add_stmt (&seq, shadow_test);
      gimple_seq_add_stmt (&seq, build_assign (BIT_AND_EXPR, base_addr, 7));
      gimple_seq_add_stmt (&seq, build_type_cast (shadow_type,
                                                  gimple_seq_last (seq)));
      if (size_in_bytes > 1)
        gimple_seq_add_stmt (&seq,
                             build_assign (PLUS_EXPR, gimple_seq_last (seq),
                                           size_in_bytes - 1));
      gimple_seq_add_stmt (&seq, build_assign (GE_EXPR, gimple_seq_last (seq),
                                               shadow));
      gimple_seq_add_stmt (&seq, build_assign (BIT_AND_EXPR, shadow_test,
                                               gimple_seq_last (seq)));
      t = gimple_assign_lhs (gimple_seq_last (seq));
      gimple_seq_set_location (seq, location);
      gsi_insert_seq_after (&gsi, seq, GSI_CONTINUE_LINKING);
    }
  else
    t = shadow;

  g = gimple_build_cond (NE_EXPR, t, build_int_cst (TREE_TYPE (t), 0),
			 NULL_TREE, NULL_TREE);
  gimple_set_location (g, location);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);

  /* Generate call to the run-time library (e.g. __asan_report_load8).  */
  gsi = gsi_start_bb (then_bb);
  g = gimple_build_call (report_error_func (is_store, size_in_bytes),
			 1, base_addr);
  gimple_set_location (g, location);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);

  *iter = gsi_start_bb (else_bb);
}

/* If T represents a memory access, add instrumentation code before ITER.
   LOCATION is source code location.
   IS_STORE is either TRUE (for a store) or FALSE (for a load).  */

static void
instrument_derefs (gimple_stmt_iterator *iter, tree t,
		   location_t location, bool is_store)
{
  tree type, base;
  HOST_WIDE_INT size_in_bytes;

  type = TREE_TYPE (t);
  switch (TREE_CODE (t))
    {
    case ARRAY_REF:
    case COMPONENT_REF:
    case INDIRECT_REF:
    case MEM_REF:
      break;
    default:
      return;
    }

  size_in_bytes = int_size_in_bytes (type);
  if ((size_in_bytes & (size_in_bytes - 1)) != 0
      || (unsigned HOST_WIDE_INT) size_in_bytes - 1 >= 16)
    return;

  HOST_WIDE_INT bitsize, bitpos;
  tree offset;
  enum machine_mode mode;
  int volatilep = 0, unsignedp = 0;
  get_inner_reference (t, &bitsize, &bitpos, &offset,
		       &mode, &unsignedp, &volatilep, false);
  if (bitpos % (size_in_bytes * BITS_PER_UNIT)
      || bitsize != size_in_bytes * BITS_PER_UNIT)
    {
      if (TREE_CODE (t) == COMPONENT_REF
	  && DECL_BIT_FIELD_REPRESENTATIVE (TREE_OPERAND (t, 1)) != NULL_TREE)
	{
	  tree repr = DECL_BIT_FIELD_REPRESENTATIVE (TREE_OPERAND (t, 1));
	  instrument_derefs (iter, build3 (COMPONENT_REF, TREE_TYPE (repr),
					   TREE_OPERAND (t, 0), repr,
					   NULL_TREE), location, is_store);
	}
      return;
    }

  base = build_fold_addr_expr (t);
  if (!has_mem_ref_been_instrumented (base, size_in_bytes))
    {
      build_check_stmt (location, base, iter, /*before_p=*/true,
			is_store, size_in_bytes);
      update_mem_ref_hash_table (base, size_in_bytes);
      update_mem_ref_hash_table (t, size_in_bytes);
    }

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

  gimple_stmt_iterator gsi = *iter;

  basic_block fallthrough_bb = NULL, then_bb = NULL;

  /* If the beginning of the memory region has already been
     instrumented, do not instrument it.  */
  bool start_instrumented = has_mem_ref_been_instrumented (base, 1);

  /* If the end of the memory region has already been instrumented, do
     not instrument it. */
  tree end = asan_mem_ref_get_end (base, len);
  bool end_instrumented = has_mem_ref_been_instrumented (end, 1);

  if (start_instrumented && end_instrumented)
    return;

  if (!is_gimple_constant (len))
    {
      /* So, the length of the memory area to asan-protect is
	 non-constant.  Let's guard the generated instrumentation code
	 like:

	 if (len != 0)
	   {
	     //asan instrumentation code goes here.
	   }
	   // falltrough instructions, starting with *ITER.  */

      gimple g = gimple_build_cond (NE_EXPR,
				    len,
				    build_int_cst (TREE_TYPE (len), 0),
				    NULL_TREE, NULL_TREE);
      gimple_set_location (g, location);
      insert_if_then_before_iter (g, iter, /*then_more_likely_p=*/true,
				  &then_bb, &fallthrough_bb);
      /* Note that fallthrough_bb starts with the statement that was
	 pointed to by ITER.  */

      /* The 'then block' of the 'if (len != 0) condition is where
	 we'll generate the asan instrumentation code now.  */
      gsi = gsi_last_bb (then_bb);
    }

  if (!start_instrumented)
    {
      /* Instrument the beginning of the memory region to be accessed,
	 and arrange for the rest of the intrumentation code to be
	 inserted in the then block *after* the current gsi.  */
      build_check_stmt (location, base, &gsi, /*before_p=*/true, is_store, 1);

      if (then_bb)
	/* We are in the case where the length of the region is not
	   constant; so instrumentation code is being generated in the
	   'then block' of the 'if (len != 0) condition.  Let's arrange
	   for the subsequent instrumentation statements to go in the
	   'then block'.  */
	gsi = gsi_last_bb (then_bb);
      else
        {
          *iter = gsi;
	  /* Don't remember this access as instrumented, if length
	     is unknown.  It might be zero and not being actually
	     instrumented, so we can't rely on it being instrumented.  */
          update_mem_ref_hash_table (base, 1);
	}
    }

  if (end_instrumented)
    return;

  /* We want to instrument the access at the end of the memory region,
     which is at (base + len - 1).  */

  /* offset = len - 1;  */
  len = unshare_expr (len);
  tree offset;
  gimple_seq seq = NULL;
  if (TREE_CODE (len) == INTEGER_CST)
    offset = fold_build2 (MINUS_EXPR, size_type_node,
			  fold_convert (size_type_node, len),
			  build_int_cst (size_type_node, 1));
  else
    {
      gimple g;
      tree t;

      if (TREE_CODE (len) != SSA_NAME)
	{
	  t = make_ssa_name (TREE_TYPE (len), NULL);
	  g = gimple_build_assign_with_ops (TREE_CODE (len), t, len, NULL);
	  gimple_set_location (g, location);
	  gimple_seq_add_stmt_without_update (&seq, g);
	  len = t;
	}
      if (!useless_type_conversion_p (size_type_node, TREE_TYPE (len)))
	{
	  t = make_ssa_name (size_type_node, NULL);
	  g = gimple_build_assign_with_ops (NOP_EXPR, t, len, NULL);
	  gimple_set_location (g, location);
	  gimple_seq_add_stmt_without_update (&seq, g);
	  len = t;
	}

      t = make_ssa_name (size_type_node, NULL);
      g = gimple_build_assign_with_ops (MINUS_EXPR, t, len,
					build_int_cst (size_type_node, 1));
      gimple_set_location (g, location);
      gimple_seq_add_stmt_without_update (&seq, g);
      offset = gimple_assign_lhs (g);
    }

  /* _1 = base;  */
  base = unshare_expr (base);
  gimple region_end =
    gimple_build_assign_with_ops (TREE_CODE (base),
				  make_ssa_name (TREE_TYPE (base), NULL),
				  base, NULL);
  gimple_set_location (region_end, location);
  gimple_seq_add_stmt_without_update (&seq, region_end);

  /* _2 = _1 + offset;  */
  region_end =
    gimple_build_assign_with_ops (POINTER_PLUS_EXPR,
				  make_ssa_name (TREE_TYPE (base), NULL),
				  gimple_assign_lhs (region_end),
				  offset);
  gimple_set_location (region_end, location);
  gimple_seq_add_stmt_without_update (&seq, region_end);
  gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);

  /* instrument access at _2;  */
  gsi = gsi_for_stmt (region_end);
  build_check_stmt (location, gimple_assign_lhs (region_end),
		    &gsi, /*before_p=*/false, is_store, 1);

  if (then_bb == NULL)
    update_mem_ref_hash_table (end, 1);

  *iter = gsi_for_stmt (gsi_stmt (*iter));
}

/* Instrument the call (to the builtin strlen function) pointed to by
   ITER.

   This function instruments the access to the first byte of the
   argument, right before the call.  After the call it instruments the
   access to the last byte of the argument; it uses the result of the
   call to deduce the offset of that last byte.

   Upon completion, iff the call has actually been instrumented, this
   function returns TRUE and *ITER points to the statement logically
   following the built-in strlen function call *ITER was initially
   pointing to.  Otherwise, the function returns FALSE and *ITER
   remains unchanged.  */

static bool
instrument_strlen_call (gimple_stmt_iterator *iter)
{
  gimple call = gsi_stmt (*iter);
  gcc_assert (is_gimple_call (call));

  tree callee = gimple_call_fndecl (call);
  gcc_assert (is_builtin_fn (callee)
	      && DECL_BUILT_IN_CLASS (callee) == BUILT_IN_NORMAL
	      && DECL_FUNCTION_CODE (callee) == BUILT_IN_STRLEN);

  tree len = gimple_call_lhs (call);
  if (len == NULL)
    /* Some passes might clear the return value of the strlen call;
       bail out in that case.  Return FALSE as we are not advancing
       *ITER.  */
    return false;
  gcc_assert (INTEGRAL_TYPE_P (TREE_TYPE (len)));

  location_t loc = gimple_location (call);
  tree str_arg = gimple_call_arg (call, 0);

  /* Instrument the access to the first byte of str_arg.  i.e:

     _1 = str_arg; instrument (_1); */
  tree cptr_type = build_pointer_type (char_type_node);
  gimple str_arg_ssa =
    gimple_build_assign_with_ops (NOP_EXPR,
				  make_ssa_name (cptr_type, NULL),
				  str_arg, NULL);
  gimple_set_location (str_arg_ssa, loc);
  gimple_stmt_iterator gsi = *iter;
  gsi_insert_before (&gsi, str_arg_ssa, GSI_NEW_STMT);
  build_check_stmt (loc, gimple_assign_lhs (str_arg_ssa), &gsi,
		    /*before_p=*/false, /*is_store=*/false, 1);

  /* If we initially had an instruction like:

	 int n = strlen (str)

     we now want to instrument the access to str[n], after the
     instruction above.*/

  /* So let's build the access to str[n] that is, access through the
     pointer_plus expr: (_1 + len).  */
  gimple stmt =
    gimple_build_assign_with_ops (POINTER_PLUS_EXPR,
				  make_ssa_name (cptr_type, NULL),
				  gimple_assign_lhs (str_arg_ssa),
				  len);
  gimple_set_location (stmt, loc);
  gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);

  build_check_stmt (loc, gimple_assign_lhs (stmt), &gsi,
		    /*before_p=*/false, /*is_store=*/false, 1);

  /* Ensure that iter points to the statement logically following the
     one it was initially pointing to.  */
  *iter = gsi;
  /* As *ITER has been advanced to point to the next statement, let's
     return true to inform transform_statements that it shouldn't
     advance *ITER anymore; otherwises it will skip that next
     statement, which wouldn't be instrumented.  */
  return true;
}

/* Instrument the call to a built-in memory access function that is
   pointed to by the iterator ITER.

   Upon completion, return TRUE iff *ITER has been advanced to the
   statement following the one it was originally pointing to.  */

static bool
instrument_builtin_call (gimple_stmt_iterator *iter)
{
  bool iter_advanced_p = false;
  gimple call = gsi_stmt (*iter);

  gcc_checking_assert (gimple_call_builtin_p (call, BUILT_IN_NORMAL));

  tree callee = gimple_call_fndecl (call);
  location_t loc = gimple_location (call);

  if (DECL_FUNCTION_CODE (callee) == BUILT_IN_STRLEN)
    iter_advanced_p = instrument_strlen_call (iter);
  else
    {
      asan_mem_ref src0, src1, dest;
      asan_mem_ref_init (&src0, NULL, 1);
      asan_mem_ref_init (&src1, NULL, 1);
      asan_mem_ref_init (&dest, NULL, 1);

      tree src0_len = NULL_TREE, src1_len = NULL_TREE, dest_len = NULL_TREE;
      bool src0_is_store = false, src1_is_store = false,
	dest_is_store = false, dest_is_deref = false;

      if (get_mem_refs_of_builtin_call (call,
					&src0, &src0_len, &src0_is_store,
					&src1, &src1_len, &src1_is_store,
					&dest, &dest_len, &dest_is_store,
					&dest_is_deref))
	{
	  if (dest_is_deref)
	    {
	      instrument_derefs (iter, dest.start, loc, dest_is_store);
	      gsi_next (iter);
	      iter_advanced_p = true;
	    }
	  else if (src0_len || src1_len || dest_len)
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
  gimple s = gsi_stmt (*iter);

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
  gimple stmt = gsi_stmt (*iter);
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
	    }
	}
      tree decl = builtin_decl_implicit (BUILT_IN_ASAN_HANDLE_NO_RETURN);
      gimple g = gimple_build_call (decl, 0);
      gimple_set_location (g, gimple_location (stmt));
      gsi_insert_before (iter, g, GSI_SAME_STMT);
    }
  return false;
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
  int saved_last_basic_block = last_basic_block;

  FOR_EACH_BB (bb)
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
	  gimple s = gsi_stmt (i);

	  if (has_stmt_been_instrumented_p (s))
	    gsi_next (&i);
	  else if (gimple_assign_single_p (s)
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
		 miss some instrumentation opportunities.  */
	      if (is_gimple_call (s) && !nonfreeing_call_p (s))
		empty_mem_ref_hash_table ();

	      gsi_next (&i);
	    }
	}
    }
  free_mem_ref_resources ();
}

/* Build
   struct __asan_global
   {
     const void *__beg;
     uptr __size;
     uptr __size_with_redzone;
     const void *__name;
     uptr __has_dynamic_init;
   } type.  */

static tree
asan_global_struct (void)
{
  static const char *field_names[5]
    = { "__beg", "__size", "__size_with_redzone",
	"__name", "__has_dynamic_init" };
  tree fields[5], ret;
  int i;

  ret = make_node (RECORD_TYPE);
  for (i = 0; i < 5; i++)
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
  TYPE_FIELDS (ret) = fields[0];
  TYPE_NAME (ret) = get_identifier ("__asan_global");
  layout_type (ret);
  return ret;
}

/* Append description of a single global DECL into vector V.
   TYPE is __asan_global struct type as returned by asan_global_struct.  */

static void
asan_add_global (tree decl, tree type, vec<constructor_elt, va_gc> *v)
{
  tree init, uptr = TREE_TYPE (DECL_CHAIN (TYPE_FIELDS (type)));
  unsigned HOST_WIDE_INT size;
  tree str_cst, refdecl = decl;
  vec<constructor_elt, va_gc> *vinner = NULL;

  pretty_printer asan_pp;

  if (DECL_NAME (decl))
    pp_tree_identifier (&asan_pp, DECL_NAME (decl));
  else
    pp_string (&asan_pp, "<unknown>");
  pp_space (&asan_pp);
  pp_left_paren (&asan_pp);
  pp_string (&asan_pp, main_input_filename);
  pp_right_paren (&asan_pp);
  str_cst = asan_pp_string (&asan_pp);

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

  CONSTRUCTOR_APPEND_ELT (vinner, NULL_TREE,
			  fold_convert (const_ptr_type_node,
					build_fold_addr_expr (refdecl)));
  size = tree_low_cst (DECL_SIZE_UNIT (decl), 1);
  CONSTRUCTOR_APPEND_ELT (vinner, NULL_TREE, build_int_cst (uptr, size));
  size += asan_red_zone_size (size);
  CONSTRUCTOR_APPEND_ELT (vinner, NULL_TREE, build_int_cst (uptr, size));
  CONSTRUCTOR_APPEND_ELT (vinner, NULL_TREE,
			  fold_convert (const_ptr_type_node, str_cst));
  CONSTRUCTOR_APPEND_ELT (vinner, NULL_TREE, build_int_cst (uptr, 0));
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
  tree BT_FN_VOID_PTR_PTR_PTR
    = build_function_type_list (void_type_node, ptr_type_node,
				ptr_type_node, ptr_type_node, NULL_TREE);
  tree BT_FN_VOID_PTR_PTRMODE
    = build_function_type_list (void_type_node, ptr_type_node,
				pointer_sized_int_node, NULL_TREE);
  tree BT_FN_VOID_INT
    = build_function_type_list (void_type_node, integer_type_node, NULL_TREE);
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
#undef ATTR_TMPURE_NORETURN_NOTHROW_LEAF_LIST
#define ATTR_TMPURE_NORETURN_NOTHROW_LEAF_LIST \
  ECF_TM_PURE | ATTR_NORETURN_NOTHROW_LEAF_LIST
#undef ATTR_COLD_NOTHROW_LEAF_LIST
#define ATTR_COLD_NOTHROW_LEAF_LIST \
  /* ECF_COLD missing */ ATTR_NOTHROW_LEAF_LIST
#undef ATTR_COLD_NORETURN_NOTHROW_LEAF_LIST
#define ATTR_COLD_NORETURN_NOTHROW_LEAF_LIST \
  /* ECF_COLD missing */ ATTR_NORETURN_NOTHROW_LEAF_LIST
#undef DEF_SANITIZER_BUILTIN
#define DEF_SANITIZER_BUILTIN(ENUM, NAME, TYPE, ATTRS) \
  decl = add_builtin_function ("__builtin_" NAME, TYPE, ENUM,		\
			       BUILT_IN_NORMAL, NAME, NULL_TREE);	\
  set_call_expr_flags (decl, ATTRS);					\
  set_builtin_decl (ENUM, decl, true);

#include "sanitizer.def"

#undef DEF_SANITIZER_BUILTIN
}

/* Called via htab_traverse.  Count number of emitted
   STRING_CSTs in the constant hash table.  */

static int
count_string_csts (void **slot, void *data)
{
  struct constant_descriptor_tree *desc
    = (struct constant_descriptor_tree *) *slot;
  if (TREE_CODE (desc->value) == STRING_CST
      && TREE_ASM_WRITTEN (desc->value)
      && asan_protect_global (desc->value))
    ++*((unsigned HOST_WIDE_INT *) data);
  return 1;
}

/* Helper structure to pass two parameters to
   add_string_csts.  */

struct asan_add_string_csts_data
{
  tree type;
  vec<constructor_elt, va_gc> *v;
};

/* Called via htab_traverse.  Call asan_add_global
   on emitted STRING_CSTs from the constant hash table.  */

static int
add_string_csts (void **slot, void *data)
{
  struct constant_descriptor_tree *desc
    = (struct constant_descriptor_tree *) *slot;
  if (TREE_CODE (desc->value) == STRING_CST
      && TREE_ASM_WRITTEN (desc->value)
      && asan_protect_global (desc->value))
    {
      struct asan_add_string_csts_data *aascd
	= (struct asan_add_string_csts_data *) data;
      asan_add_global (SYMBOL_REF_DECL (XEXP (desc->rtl, 0)),
		       aascd->type, aascd->v);
    }
  return 1;
}

/* Needs to be GTY(()), because cgraph_build_static_cdtor may
   invoke ggc_collect.  */
static GTY(()) tree asan_ctor_statements;

/* Module-level instrumentation.
   - Insert __asan_init() into the list of CTORs.
   - TODO: insert redzones around globals.
 */

void
asan_finish_file (void)
{
  struct varpool_node *vnode;
  unsigned HOST_WIDE_INT gcount = 0;

  if (shadow_ptr_types[0] == NULL_TREE)
    asan_init_shadow_ptr_types ();
  /* Avoid instrumenting code in the asan ctors/dtors.
     We don't need to insert padding after the description strings,
     nor after .LASAN* array.  */
  flag_sanitize &= ~SANITIZE_ADDRESS;

  tree fn = builtin_decl_implicit (BUILT_IN_ASAN_INIT);
  append_to_statement_list (build_call_expr (fn, 0), &asan_ctor_statements);
  FOR_EACH_DEFINED_VARIABLE (vnode)
    if (TREE_ASM_WRITTEN (vnode->symbol.decl)
	&& asan_protect_global (vnode->symbol.decl))
      ++gcount;
  htab_t const_desc_htab = constant_pool_htab ();
  htab_traverse (const_desc_htab, count_string_csts, &gcount);
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
	if (TREE_ASM_WRITTEN (vnode->symbol.decl)
	    && asan_protect_global (vnode->symbol.decl))
	  asan_add_global (vnode->symbol.decl, TREE_TYPE (type), v);
      struct asan_add_string_csts_data aascd;
      aascd.type = TREE_TYPE (type);
      aascd.v = v;
      htab_traverse (const_desc_htab, add_string_csts, &aascd);
      ctor = build_constructor (type, v);
      TREE_CONSTANT (ctor) = 1;
      TREE_STATIC (ctor) = 1;
      DECL_INITIAL (var) = ctor;
      varpool_assemble_decl (varpool_node_for_decl (var));

      fn = builtin_decl_implicit (BUILT_IN_ASAN_REGISTER_GLOBALS);
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
      cgraph_build_static_cdtor ('D', dtor_statements,
				 MAX_RESERVED_INIT_PRIORITY - 1);
    }
  cgraph_build_static_cdtor ('I', asan_ctor_statements,
			     MAX_RESERVED_INIT_PRIORITY - 1);
  flag_sanitize |= SANITIZE_ADDRESS;
}

/* Instrument the current function.  */

static unsigned int
asan_instrument (void)
{
  if (shadow_ptr_types[0] == NULL_TREE)
    asan_init_shadow_ptr_types ();
  transform_statements ();
  return 0;
}

static bool
gate_asan (void)
{
  return (flag_sanitize & SANITIZE_ADDRESS) != 0
	  && !lookup_attribute ("no_sanitize_address",
				DECL_ATTRIBUTES (current_function_decl));
}

namespace {

const pass_data pass_data_asan =
{
  GIMPLE_PASS, /* type */
  "asan", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_NONE, /* tv_id */
  ( PROP_ssa | PROP_cfg | PROP_gimple_leh ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_verify_flow | TODO_verify_stmts
    | TODO_update_ssa ), /* todo_flags_finish */
};

class pass_asan : public gimple_opt_pass
{
public:
  pass_asan (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_asan, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_asan (m_ctxt); }
  bool gate () { return gate_asan (); }
  unsigned int execute () { return asan_instrument (); }

}; // class pass_asan

} // anon namespace

gimple_opt_pass *
make_pass_asan (gcc::context *ctxt)
{
  return new pass_asan (ctxt);
}

static bool
gate_asan_O0 (void)
{
  return !optimize && gate_asan ();
}

namespace {

const pass_data pass_data_asan_O0 =
{
  GIMPLE_PASS, /* type */
  "asan0", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_NONE, /* tv_id */
  ( PROP_ssa | PROP_cfg | PROP_gimple_leh ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_verify_flow | TODO_verify_stmts
    | TODO_update_ssa ), /* todo_flags_finish */
};

class pass_asan_O0 : public gimple_opt_pass
{
public:
  pass_asan_O0 (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_asan_O0, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_asan_O0 (); }
  unsigned int execute () { return asan_instrument (); }

}; // class pass_asan_O0

} // anon namespace

gimple_opt_pass *
make_pass_asan_O0 (gcc::context *ctxt)
{
  return new pass_asan_O0 (ctxt);
}

#include "gt-asan.h"
