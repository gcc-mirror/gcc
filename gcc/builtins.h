/* Expand builtin functions.
   Copyright (C) 1988-2021 Free Software Foundation, Inc.

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

#ifndef GCC_BUILTINS_H
#define GCC_BUILTINS_H

#include <mpc.h>

/* Target-dependent globals.  */
struct target_builtins {
  /* For each register that may be used for calling a function, this
     gives a mode used to copy the register's value.  VOIDmode indicates
     the register is not used for calling a function.  If the machine
     has register windows, this gives only the outbound registers.
     INCOMING_REGNO gives the corresponding inbound register.  */
  fixed_size_mode_pod x_apply_args_mode[FIRST_PSEUDO_REGISTER];

  /* For each register that may be used for returning values, this gives
     a mode used to copy the register's value.  VOIDmode indicates the
     register is not used for returning values.  If the machine has
     register windows, this gives only the outbound registers.
     INCOMING_REGNO gives the corresponding inbound register.  */
  fixed_size_mode_pod x_apply_result_mode[FIRST_PSEUDO_REGISTER];
};

extern struct target_builtins default_target_builtins;
#if SWITCHABLE_TARGET
extern struct target_builtins *this_target_builtins;
#else
#define this_target_builtins (&default_target_builtins)
#endif

/* Non-zero if __builtin_constant_p should be folded right away.  */
extern bool force_folding_builtin_constant_p;

extern bool called_as_built_in (tree);
extern bool get_object_alignment_1 (tree, unsigned int *,
				    unsigned HOST_WIDE_INT *);
extern unsigned int get_object_alignment (tree);
extern bool get_pointer_alignment_1 (tree, unsigned int *,
				     unsigned HOST_WIDE_INT *);
extern unsigned int get_pointer_alignment (tree);
extern unsigned string_length (const void*, unsigned, unsigned);

struct c_strlen_data
{
  /* [MINLEN, MAXBOUND, MAXLEN] is a range describing the length of
     one or more strings of possibly unknown length.  For a single
     string of known length the range is a constant where
     MINLEN == MAXBOUND == MAXLEN holds.
     For other strings, MINLEN is the length of the shortest known
     string.  MAXBOUND is the length of a string that could be stored
     in the largest array referenced by the expression.  MAXLEN is
     the length of the longest sequence of non-zero bytes
     in an object referenced by the expression.  For such strings,
     MINLEN <= MAXBOUND <= MAXLEN holds.  For example, given:
       struct A { char a[7], b[]; };
       extern struct A *p;
       n = strlen (p->a);
     the computed range will be [0, 6, ALL_ONES].
     However, for a conditional expression involving a string
     of known length and an array of unknown bound such as
       n = strlen (i ? p->b : "123");
     the range will be [3, 3, ALL_ONES].
     MINLEN != 0 && MAXLEN == ALL_ONES indicates that MINLEN is
     the length of the shortest known string and implies that
     the shortest possible string referenced by the expression may
     actually be the empty string.  This distinction is useful for
     diagnostics.  get_range_strlen() return value distinguishes
     between these two cases.
     As the tighter (and more optimistic) bound, MAXBOUND is suitable
     for diagnostics but not for optimization.
     As the more conservative bound, MAXLEN is intended to be used
     for optimization.  */
  tree minlen;
  tree maxlen;
  tree maxbound;
  /* When non-null, DECL refers to the declaration known to store
     an unterminated constant character array, as in:
     const char s[] = { 'a', 'b', 'c' };
     It is used to diagnose uses of such arrays in functions such as
     strlen() that expect a nul-terminated string as an argument.  */
  tree decl;
  /* Non-constant offset from the beginning of a string not accounted
     for in the length range.  Used to improve diagnostics.  */
  tree off;
};

extern tree c_strlen (tree, int, c_strlen_data * = NULL, unsigned = 1);
extern rtx c_readstr (const char *, scalar_int_mode, bool = true);
extern void expand_builtin_setjmp_setup (rtx, rtx);
extern void expand_builtin_setjmp_receiver (rtx);
extern void expand_builtin_update_setjmp_buf (rtx);
extern tree mathfn_built_in (tree, enum built_in_function fn);
extern tree mathfn_built_in (tree, combined_fn);
extern tree mathfn_built_in_type (combined_fn);
extern rtx builtin_strncpy_read_str (void *, void *, HOST_WIDE_INT,
				     scalar_int_mode);
extern rtx builtin_memset_read_str (void *, void *, HOST_WIDE_INT,
				    scalar_int_mode);
extern rtx expand_builtin_saveregs (void);
extern tree std_build_builtin_va_list (void);
extern tree std_fn_abi_va_list (tree);
extern tree std_canonical_va_list_type (tree);
extern void std_expand_builtin_va_start (tree, rtx);
extern void expand_builtin_trap (void);
extern void expand_ifn_atomic_bit_test_and (gcall *);
extern void expand_ifn_atomic_compare_exchange (gcall *);
extern rtx expand_builtin (tree, rtx, rtx, machine_mode, int);
extern enum built_in_function builtin_mathfn_code (const_tree);
extern tree fold_builtin_expect (location_t, tree, tree, tree, tree);
extern bool avoid_folding_inline_builtin (tree);
extern tree fold_call_expr (location_t, tree, bool);
extern tree fold_builtin_call_array (location_t, tree, tree, int, tree *);
extern bool validate_gimple_arglist (const gcall *, ...);
extern rtx default_expand_builtin (tree, rtx, rtx, machine_mode, int);
extern void maybe_emit_call_builtin___clear_cache (rtx, rtx);
extern bool fold_builtin_next_arg (tree, bool);
extern tree do_mpc_arg2 (tree, tree, tree, int, int (*)(mpc_ptr, mpc_srcptr, mpc_srcptr, mpc_rnd_t));
extern tree fold_call_stmt (gcall *, bool);
extern void set_builtin_user_assembler_name (tree decl, const char *asmspec);
extern bool is_simple_builtin (tree);
extern bool is_inexpensive_builtin (tree);
extern bool readonly_data_expr (tree exp);
extern bool init_target_chars (void);
extern unsigned HOST_WIDE_INT target_newline;
extern unsigned HOST_WIDE_INT target_percent;
extern char target_percent_s[3];
extern char target_percent_c[3];
extern char target_percent_s_newline[4];
extern bool target_char_cst_p (tree t, char *p);

extern internal_fn associated_internal_fn (tree);
extern internal_fn replacement_internal_fn (gcall *);

extern bool check_nul_terminated_array (tree, tree, tree = NULL_TREE);
extern void warn_string_no_nul (location_t, tree, const char *, tree,
				tree, tree = NULL_TREE, bool = false,
				const wide_int[2] = NULL);
extern tree unterminated_array (tree, tree * = NULL, bool * = NULL);
extern bool builtin_with_linkage_p (tree);

/* Describes recursion limits used by functions that follow use-def
   chains of SSA_NAMEs.  */

class ssa_name_limit_t
{
  bitmap visited;         /* Bitmap of visited SSA_NAMEs.  */
  unsigned ssa_def_max;   /* Longest chain of SSA_NAMEs to follow.  */

  /* Not copyable or assignable.  */
  DISABLE_COPY_AND_ASSIGN (ssa_name_limit_t);

public:

  ssa_name_limit_t ()
    : visited (),
      ssa_def_max (param_ssa_name_def_chain_limit) { }

  /* Set a bit for the PHI in VISITED and return true if it wasn't
     already set.  */
  bool visit_phi (tree);
  /* Clear a bit for the PHI in VISITED.  */
  void leave_phi (tree);
  /* Return false if the SSA_NAME chain length counter has reached
     the limit, otherwise increment the counter and return true.  */
  bool next ();

  /* If the SSA_NAME has already been "seen" return a positive value.
     Otherwise add it to VISITED.  If the SSA_NAME limit has been
     reached, return a negative value.  Otherwise return zero.  */
  int next_phi (tree);

  ~ssa_name_limit_t ();
};

class pointer_query;

/* Describes a reference to an object used in an access.  */
struct access_ref
{
  /* Set the bounds of the reference to at most as many bytes
     as the first argument or unknown when null, and at least
     one when the second argument is true unless the first one
     is a constant zero.  */
  access_ref (tree = NULL_TREE, bool = false);

  /* Return the PHI node REF refers to or null if it doesn't.  */
  gphi *phi () const;

  /* Return the object to which REF refers.  */
  tree get_ref (vec<access_ref> *, access_ref * = NULL, int = 1,
		ssa_name_limit_t * = NULL, pointer_query * = NULL) const;

  /* Return true if OFFRNG is the constant zero.  */
  bool offset_zero () const
  {
    return offrng[0] == 0 && offrng[1] == 0;
  }

  /* Return true if OFFRNG is bounded to a subrange of offset values
     valid for the largest possible object.  */
  bool offset_bounded () const;

  /* Return the maximum amount of space remaining and if non-null, set
     argument to the minimum.  */
  offset_int size_remaining (offset_int * = NULL) const;

/* Return true if the offset and object size are in range for SIZE.  */
  bool offset_in_range (const offset_int &) const;

  /* Return true if *THIS is an access to a declared object.  */
  bool ref_declared () const
  {
    return DECL_P (ref) && base0 && deref < 1;
  }

  /* Set the size range to the maximum.  */
  void set_max_size_range ()
  {
    sizrng[0] = 0;
    sizrng[1] = wi::to_offset (max_object_size ());
  }

  /* Add OFF to the offset range.  */
  void add_offset (const offset_int &off)
  {
    add_offset (off, off);
  }

  /* Add the range [MIN, MAX] to the offset range.  */
  void add_offset (const offset_int &, const offset_int &);

  /* Add the maximum representable offset to the offset range.  */
  void add_max_offset ()
  {
    offset_int maxoff = wi::to_offset (TYPE_MAX_VALUE (ptrdiff_type_node));
    add_offset (-maxoff - 1, maxoff);
  }

  /* Issue an informational message describing the target of an access
     with the given mode.  */
  void inform_access (access_mode) const;

  /* Reference to the accessed object(s).  */
  tree ref;

  /* Range of byte offsets into and sizes of the object(s).  */
  offset_int offrng[2];
  offset_int sizrng[2];
  /* The minimum and maximum offset computed.  */
  offset_int offmax[2];
  /* Range of the bound of the access: denotes that the access
     is at least BNDRNG[0] bytes but no more than BNDRNG[1].
     For string functions the size of the actual access is
     further constrained by the length of the string.  */
  offset_int bndrng[2];

  /* Used to fold integer expressions when called from front ends.  */
  tree (*eval)(tree);
  /* Positive when REF is dereferenced, negative when its address is
     taken.  */
  int deref;
  /* Set if trailing one-element arrays should be treated as flexible
     array members.  */
  bool trail1special;
  /* Set if valid offsets must start at zero (for declared and allocated
     objects but not for others referenced by pointers).  */
  bool base0;
  /* Set if REF refers to a function array parameter not declared
     static.  */
  bool parmarray;
};

class range_query;

/* Queries and caches compute_objsize results.  */
class pointer_query
{
  DISABLE_COPY_AND_ASSIGN (pointer_query);

public:
  /* Type of the two-level cache object defined by clients of the class
     to have pointer SSA_NAMEs cached for speedy access.  */
  struct cache_type
  {
    /* 1-based indices into cache.  */
    vec<unsigned> indices;
    /* The cache itself.  */
    vec<access_ref> access_refs;
  };

  /* Construct an object with the given Ranger instance and cache.  */
  explicit pointer_query (range_query * = NULL, cache_type * = NULL);

  /* Retrieve the access_ref for a variable from cache if it's there.  */
  const access_ref* get_ref (tree, int = 1) const;

  /* Retrieve the access_ref for a variable from cache or compute it.  */
  bool get_ref (tree, access_ref*, int = 1);

  /* Add an access_ref for the SSA_NAME to the cache.  */
  void put_ref (tree, const access_ref&, int = 1);

  /* Flush the cache.  */
  void flush_cache ();

  /* A Ranger instance.  May be null to use global ranges.  */
  range_query *rvals;
  /* Cache of SSA_NAMEs.  May be null to disable caching.  */
  cache_type *var_cache;

  /* Cache performance counters.  */
  mutable unsigned hits;
  mutable unsigned misses;
  mutable unsigned failures;
  mutable unsigned depth;
  mutable unsigned max_depth;
};

/* Describes a pair of references used in an access by built-in
   functions like memcpy.  */
struct access_data
{
  /* Set the access to at most MAXWRITE and MAXREAD bytes, and
     at least 1 when MINWRITE or MINREAD, respectively, is set.  */
  access_data (tree expr, access_mode mode,
	       tree maxwrite = NULL_TREE, bool minwrite = false,
	       tree maxread = NULL_TREE, bool minread = false)
    : call (expr),
      dst (maxwrite, minwrite), src (maxread, minread), mode (mode) { }

  /* Built-in function call.  */
  tree call;
  /* Destination and source of the access.  */
  access_ref dst, src;
  /* Read-only for functions like memcmp or strlen, write-only
     for memset, read-write for memcpy or strcat.  */
  access_mode mode;
};

extern tree gimple_call_alloc_size (gimple *, wide_int[2] = NULL,
				    range_query * = NULL);
extern tree gimple_parm_array_size (tree, wide_int[2], bool * = NULL);

extern tree compute_objsize (tree, int, access_ref *, range_query * = NULL);
/* Legacy/transitional API.  Should not be used in new code.  */
extern tree compute_objsize (tree, int, access_ref *, pointer_query *);
extern tree compute_objsize (tree, int, tree * = NULL, tree * = NULL,
			     range_query * = NULL);
extern bool check_access (tree, tree, tree, tree, tree,
			  access_mode, const access_data * = NULL);
extern void maybe_emit_free_warning (tree);

#endif /* GCC_BUILTINS_H */
