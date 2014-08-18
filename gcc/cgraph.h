/* Callgraph handling code.
   Copyright (C) 2003-2014 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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

#ifndef GCC_CGRAPH_H
#define GCC_CGRAPH_H

#include "hash-map.h"
#include "is-a.h"
#include "plugin-api.h"
#include "vec.h"
#include "basic-block.h"
#include "function.h"
#include "ipa-ref.h"

/* Symbol table consists of functions and variables.
   TODO: add labels and CONST_DECLs.  */
enum symtab_type
{
  SYMTAB_SYMBOL,
  SYMTAB_FUNCTION,
  SYMTAB_VARIABLE
};

/* Section names are stored as reference counted strings in GGC safe hashtable
   (to make them survive through PCH).  */

struct GTY(()) section_hash_entry_d
{
  int ref_count;
  char *name;  /* As long as this datastructure stays in GGC, we can not put
		  string at the tail of structure of GGC dies in horrible
		  way  */
};

typedef struct section_hash_entry_d section_hash_entry;

enum availability
{
  /* Not yet set by cgraph_function_body_availability.  */
  AVAIL_UNSET,
  /* Function body/variable initializer is unknown.  */
  AVAIL_NOT_AVAILABLE,
  /* Function body/variable initializer is known but might be replaced
     by a different one from other compilation unit and thus needs to
     be dealt with a care.  Like AVAIL_NOT_AVAILABLE it can have
     arbitrary side effects on escaping variables and functions, while
     like AVAILABLE it might access static variables.  */
  AVAIL_INTERPOSABLE,
  /* Function body/variable initializer is known and will be used in final
     program.  */
  AVAIL_AVAILABLE,
  /* Function body/variable initializer is known and all it's uses are
     explicitly visible within current unit (ie it's address is never taken and
     it is not exported to other units). Currently used only for functions.  */
  AVAIL_LOCAL
};

/* Classification of symbols WRT partitioning.  */
enum symbol_partitioning_class
{
   /* External declarations are ignored by partitioning algorithms and they are
      added into the boundary later via compute_ltrans_boundary.  */
   SYMBOL_EXTERNAL,
   /* Partitioned symbols are pur into one of partitions.  */
   SYMBOL_PARTITION,
   /* Duplicated symbols (such as comdat or constant pool references) are
      copied into every node needing them via add_symbol_to_partition.  */
   SYMBOL_DUPLICATE
};

/* Base of all entries in the symbol table.
   The symtab_node is inherited by cgraph and varpol nodes.  */
class GTY((desc ("%h.type"), tag ("SYMTAB_SYMBOL"),
	   chain_next ("%h.next"), chain_prev ("%h.previous")))
  symtab_node
{
public:
  /* Return name.  */
  const char *name () const;

  /* Return asm name.  */
  const char * asm_name () const;

  /* Add node into symbol table.  This function is not used directly, but via
     cgraph/varpool node creation routines.  */
  void register_symbol (void);

  /* Remove symbol from symbol table.  */
  void remove (void);

  /* Dump symtab node to F.  */
  void dump (FILE *f);

  /* Dump symtab node to stderr.  */
  void DEBUG_FUNCTION debug (void);

  /* Verify consistency of node.  */
  void DEBUG_FUNCTION verify (void);

  /* Return ipa reference from this symtab_node to
     REFERED_NODE or REFERED_VARPOOL_NODE. USE_TYPE specify type
     of the use and STMT the statement (if it exists).  */
  struct ipa_ref *add_reference (symtab_node *referred_node,
				 enum ipa_ref_use use_type);

  /* Return ipa reference from this symtab_node to
     REFERED_NODE or REFERED_VARPOOL_NODE. USE_TYPE specify type
     of the use and STMT the statement (if it exists).  */
  struct ipa_ref *add_reference (symtab_node *referred_node,
				 enum ipa_ref_use use_type, gimple stmt);

  /* If VAL is a reference to a function or a variable, add a reference from
     this symtab_node to the corresponding symbol table node.  USE_TYPE specify
     type of the use and STMT the statement (if it exists).  Return the new
     reference or NULL if none was created.  */
  struct ipa_ref *maybe_add_reference (tree val, enum ipa_ref_use use_type,
				       gimple stmt);

  /* Clone all references from symtab NODE to this symtab_node.  */
  void clone_references (symtab_node *node);

  /* Remove all stmt references in non-speculative references.
     Those are not maintained during inlining & clonning.
     The exception are speculative references that are updated along
     with callgraph edges associated with them.  */
  void clone_referring (symtab_node *node);

  /* Clone reference REF to this symtab_node and set its stmt to STMT.  */
  struct ipa_ref *clone_reference (struct ipa_ref *ref, gimple stmt);

  /* Find the structure describing a reference to REFERRED_NODE
     and associated with statement STMT.  */
  struct ipa_ref *find_reference (symtab_node *referred_node, gimple stmt,
				  unsigned int lto_stmt_uid);

  /* Remove all references that are associated with statement STMT.  */
  void remove_stmt_references (gimple stmt);

  /* Remove all stmt references in non-speculative references.
     Those are not maintained during inlining & clonning.
     The exception are speculative references that are updated along
     with callgraph edges associated with them.  */
  void clear_stmts_in_references (void);

  /* Remove all references in ref list.  */
  void remove_all_references (void);

  /* Remove all referring items in ref list.  */
  void remove_all_referring (void);

  /* Dump references in ref list to FILE.  */
  void dump_references (FILE *file);

  /* Dump referring in list to FILE.  */
  void dump_referring (FILE *);

  /* Iterates I-th reference in the list, REF is also set.  */
  struct ipa_ref *iterate_reference (unsigned i, struct ipa_ref *&ref);

  /* Iterates I-th referring item in the list, REF is also set.  */
  struct ipa_ref *iterate_referring (unsigned i, struct ipa_ref *&ref);

  /* Iterates I-th referring alias item in the list, REF is also set.  */
  struct ipa_ref *iterate_direct_aliases (unsigned i, struct ipa_ref *&ref);

  /* Return true if symtab node and TARGET represents
     semantically equivalent symbols.  */
  bool semantically_equivalent_p (symtab_node *target);

  /* Classify symbol symtab node for partitioning.  */
  enum symbol_partitioning_class get_partitioning_class (void);

  /* Return comdat group.  */
  tree get_comdat_group ()
    {
      return x_comdat_group;
    }

  /* Return comdat group as identifier_node.  */
  tree get_comdat_group_id ()
    {
      if (x_comdat_group && TREE_CODE (x_comdat_group) != IDENTIFIER_NODE)
	x_comdat_group = DECL_ASSEMBLER_NAME (x_comdat_group);
      return x_comdat_group;
    }

  /* Set comdat group.  */
  void set_comdat_group (tree group)
    {
      gcc_checking_assert (!group || TREE_CODE (group) == IDENTIFIER_NODE
			   || DECL_P (group));
      x_comdat_group = group;
    }

  /* Return section as string.  */
  const char * get_section ()
    {
      if (!x_section)
	return NULL;
      return x_section->name;
    }

  /* Remove node from same comdat group.   */
  void remove_from_same_comdat_group (void);

  /* Add this symtab_node to the same comdat group that OLD is in.  */
  void add_to_same_comdat_group (symtab_node *old_node);

  /* Dissolve the same_comdat_group list in which NODE resides.  */
  void dissolve_same_comdat_group_list (void);

  /* Return true when symtab_node is known to be used from other (non-LTO)
     object file. Known only when doing LTO via linker plugin.  */
  bool used_from_object_file_p (void);

  /* Walk the alias chain to return the symbol NODE is alias of.
     If NODE is not an alias, return NODE.
     When AVAILABILITY is non-NULL, get minimal availability in the chain.  */
  symtab_node *ultimate_alias_target (enum availability *avail = NULL);

  /* Return next reachable static symbol with initializer after NODE.  */
  inline symtab_node *next_defined_symbol (void);

  /* Add reference recording that symtab node is alias of TARGET.
     The function can fail in the case of aliasing cycles; in this case
     it returns false.  */
  bool resolve_alias (symtab_node *target);

  /* C++ FE sometimes change linkage flags after producing same
     body aliases.  */
  void fixup_same_cpp_alias_visibility (symtab_node *target);

  /* Call calback on symtab node and aliases associated to this node.
     When INCLUDE_OVERWRITABLE is false, overwritable aliases and thunks are
     skipped. */
  bool call_for_symbol_and_aliases (bool (*callback) (symtab_node *, void *),
				  void *data,
				  bool include_overwrite);

  /* If node can not be interposable by static or dynamic linker to point to
     different definition, return this symbol. Otherwise look for alias with
     such property and if none exists, introduce new one.  */
  symtab_node *noninterposable_alias (void);

  /* Return node that alias is aliasing.  */
  inline symtab_node *get_alias_target (void);

  /* Set section for symbol and its aliases.  */
  void set_section (const char *section);

  /* Set section, do not recurse into aliases.
     When one wants to change section of symbol and its aliases,
     use set_section.  */
  void set_section_for_node (const char *section);

  /* Set initialization priority to PRIORITY.  */
  void set_init_priority (priority_type priority);

  /* Return the initialization priority.  */
  priority_type get_init_priority ();

  /* Return availability of NODE.  */
  enum availability get_availability (void);

  /* Make DECL local.  */
  void make_decl_local (void);

  /* Return true if list contains an alias.  */
  bool has_aliases_p (void);

  /* Return true when the symbol is real symbol, i.e. it is not inline clone
     or abstract function kept for debug info purposes only.  */
  bool real_symbol_p (void);

  /* Return true if NODE can be discarded by linker from the binary.  */
  inline bool
  can_be_discarded_p (void)
  {
    return (DECL_EXTERNAL (decl)
	    || (get_comdat_group ()
		&& resolution != LDPR_PREVAILING_DEF
		&& resolution != LDPR_PREVAILING_DEF_IRONLY
		&& resolution != LDPR_PREVAILING_DEF_IRONLY_EXP));
  }

  /* Return true if NODE is local to a particular COMDAT group, and must not
     be named from outside the COMDAT.  This is used for C++ decloned
     constructors.  */
  inline bool comdat_local_p (void)
  {
    return (same_comdat_group && !TREE_PUBLIC (decl));
  }

  /* Return true if ONE and TWO are part of the same COMDAT group.  */
  inline bool in_same_comdat_group_p (symtab_node *target);

  /* Return true when there is a reference to node and it is not vtable.  */
  bool address_taken_from_non_vtable_p (void);

  /* Return true if symbol is known to be nonzero.  */
  bool nonzero_address ();

  /* Return symbol table node associated with DECL, if any,
     and NULL otherwise.  */
  static inline symtab_node *get (const_tree decl)
  {
#ifdef ENABLE_CHECKING
    /* Check that we are called for sane type of object - functions
       and static or external variables.  */
    gcc_checking_assert (TREE_CODE (decl) == FUNCTION_DECL
			 || (TREE_CODE (decl) == VAR_DECL
			     && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)
				 || in_lto_p)));
    /* Check that the mapping is sane - perhaps this check can go away,
       but at the moment frontends tends to corrupt the mapping by calling
       memcpy/memset on the tree nodes.  */
    gcc_checking_assert (!decl->decl_with_vis.symtab_node
			 || decl->decl_with_vis.symtab_node->decl == decl);
#endif
    return decl->decl_with_vis.symtab_node;
  }

  /* Dump symbol table to F.  */
  static void dump_table (FILE *);

  /* Dump symbol table to stderr.  */
  static inline DEBUG_FUNCTION void debug_symtab (void)
  {
    dump_table (stderr);
  }

  /* Verify symbol table for internal consistency.  */
  static DEBUG_FUNCTION void verify_symtab_nodes (void);

  /* Return true when NODE is known to be used from other (non-LTO)
     object file. Known only when doing LTO via linker plugin.  */
  static bool used_from_object_file_p_worker (symtab_node *node);

  /* Type of the symbol.  */
  ENUM_BITFIELD (symtab_type) type : 8;

  /* The symbols resolution.  */
  ENUM_BITFIELD (ld_plugin_symbol_resolution) resolution : 8;

  /*** Flags representing the symbol type.  ***/

  /* True when symbol corresponds to a definition in current unit.
     set via cgraph_finalize_function or varpool_finalize_decl  */
  unsigned definition : 1;
  /* True when symbol is an alias.
     Set by ssemble_alias.  */
  unsigned alias : 1;
  /* True when alias is a weakref.  */
  unsigned weakref : 1;
  /* C++ frontend produce same body aliases and extra name aliases for
     virtual functions and vtables that are obviously equivalent.
     Those aliases are bit special, especially because C++ frontend
     visibility code is so ugly it can not get them right at first time
     and their visibility needs to be copied from their "masters" at
     the end of parsing.  */
  unsigned cpp_implicit_alias : 1;
  /* Set once the definition was analyzed.  The list of references and
     other properties are built during analysis.  */
  unsigned analyzed : 1;
  /* Set for write-only variables.  */
  unsigned writeonly : 1;


  /*** Visibility and linkage flags.  ***/

  /* Set when function is visible by other units.  */
  unsigned externally_visible : 1;
  /* The symbol will be assumed to be used in an invisible way (like
     by an toplevel asm statement).  */
  unsigned force_output : 1;
  /* Like FORCE_OUTPUT, but in the case it is ABI requiring the symbol to be
     exported.  Unlike FORCE_OUTPUT this flag gets cleared to symbols promoted
     to static and it does not inhibit optimization.  */
  unsigned forced_by_abi : 1;
  /* True when the name is known to be unique and thus it does not need mangling.  */
  unsigned unique_name : 1;
  /* Specify whether the section was set by user or by
     compiler via -ffunction-sections.  */
  unsigned implicit_section : 1;
  /* True when body and other characteristics have been removed by
     symtab_remove_unreachable_nodes. */
  unsigned body_removed : 1;

  /*** WHOPR Partitioning flags.
       These flags are used at ltrans stage when only part of the callgraph is
       available. ***/

  /* Set when variable is used from other LTRANS partition.  */
  unsigned used_from_other_partition : 1;
  /* Set when function is available in the other LTRANS partition.
     During WPA output it is used to mark nodes that are present in
     multiple partitions.  */
  unsigned in_other_partition : 1;



  /*** other flags.  ***/

  /* Set when symbol has address taken. */
  unsigned address_taken : 1;
  /* Set when init priority is set.  */
  unsigned in_init_priority_hash : 1;


  /* Ordering of all symtab entries.  */
  int order;

  /* Declaration representing the symbol.  */
  tree decl;

  /* Linked list of symbol table entries starting with symtab_nodes.  */
  symtab_node *next;
  symtab_node *previous;

  /* Linked list of symbols with the same asm name.  There may be multiple
     entries for single symbol name during LTO, because symbols are renamed
     only after partitioning.

     Because inline clones are kept in the assembler name has, they also produce
     duplicate entries.

     There are also several long standing bugs where frontends and builtin
     code produce duplicated decls.  */
  symtab_node *next_sharing_asm_name;
  symtab_node *previous_sharing_asm_name;

  /* Circular list of nodes in the same comdat group if non-NULL.  */
  symtab_node *same_comdat_group;

  /* Vectors of referring and referenced entities.  */
  struct ipa_ref_list ref_list;

  /* Alias target. May be either DECL pointer or ASSEMBLER_NAME pointer
     depending to what was known to frontend on the creation time.
     Once alias is resolved, this pointer become NULL.  */
  tree alias_target;

  /* File stream where this node is being written to.  */
  struct lto_file_decl_data * lto_file_data;

  PTR GTY ((skip)) aux;

  /* Comdat group the symbol is in.  Can be private if GGC allowed that.  */
  tree x_comdat_group;

  /* Section name. Again can be private, if allowed.  */
  section_hash_entry *x_section;

protected:
  /* Dump base fields of symtab nodes to F.  Not to be used directly.  */
  void dump_base (FILE *);

  /* Verify common part of symtab node.  */
  bool DEBUG_FUNCTION verify_base (void);

  /* Remove node from symbol table.  This function is not used directly, but via
     cgraph/varpool node removal routines.  */
  void unregister (void);

  /* Return the initialization and finalization priority information for
     DECL.  If there is no previous priority information, a freshly
     allocated structure is returned.  */
  struct symbol_priority_map *priority_info (void);

private:
  /* Worker for set_section.  */
  static bool set_section (symtab_node *n, void *s);

  /* Worker for symtab_resolve_alias.  */
  static bool set_implicit_section (symtab_node *n, void *);

  /* Worker searching noninterposable alias.  */
  static bool noninterposable_alias (symtab_node *node, void *data);
};

/* Walk all aliases for NODE.  */
#define FOR_EACH_ALIAS(node, alias) \
  for (unsigned x_i = 0; node->iterate_direct_aliases (x_i, alias); x_i++)

/* This is the information that is put into the cgraph local structure
   to recover a function.  */
struct lto_file_decl_data;

extern const char * const cgraph_availability_names[];
extern const char * const ld_plugin_symbol_resolution_names[];
extern const char * const tls_model_names[];

/* Information about thunk, used only for same body aliases.  */

struct GTY(()) cgraph_thunk_info {
  /* Information about the thunk.  */
  HOST_WIDE_INT fixed_offset;
  HOST_WIDE_INT virtual_value;
  tree alias;
  bool this_adjusting;
  bool virtual_offset_p;
  /* Set to true when alias node is thunk.  */
  bool thunk_p;
};

/* Information about the function collected locally.
   Available after function is analyzed.  */

struct GTY(()) cgraph_local_info {
  /* Set when function function is visible in current compilation unit only
     and its address is never taken.  */
  unsigned local : 1;

  /* False when there is something makes versioning impossible.  */
  unsigned versionable : 1;

  /* False when function calling convention and signature can not be changed.
     This is the case when __builtin_apply_args is used.  */
  unsigned can_change_signature : 1;

  /* True when the function has been originally extern inline, but it is
     redefined now.  */
  unsigned redefined_extern_inline : 1;

  /* True if the function may enter serial irrevocable mode.  */
  unsigned tm_may_enter_irr : 1;
};

/* Information about the function that needs to be computed globally
   once compilation is finished.  Available only with -funit-at-a-time.  */

struct GTY(()) cgraph_global_info {
  /* For inline clones this points to the function they will be
     inlined into.  */
  cgraph_node *inlined_to;
};

/* Information about the function that is propagated by the RTL backend.
   Available only for functions that has been already assembled.  */

struct GTY(()) cgraph_rtl_info {
   unsigned int preferred_incoming_stack_boundary;

  /* Call unsaved hard registers really used by the corresponding
     function (including ones used by functions called by the
     function).  */
  HARD_REG_SET function_used_regs;
  /* Set if function_used_regs is valid.  */
  unsigned function_used_regs_valid: 1;
};

/* Represent which DECL tree (or reference to such tree)
   will be replaced by another tree while versioning.  */
struct GTY(()) ipa_replace_map
{
  /* The tree that will be replaced.  */
  tree old_tree;
  /* The new (replacing) tree.  */
  tree new_tree;
  /* Parameter number to replace, when old_tree is NULL.  */
  int parm_num;
  /* True when a substitution should be done, false otherwise.  */
  bool replace_p;
  /* True when we replace a reference to old_tree.  */
  bool ref_p;
};

struct GTY(()) cgraph_clone_info
{
  vec<ipa_replace_map *, va_gc> *tree_map;
  bitmap args_to_skip;
  bitmap combined_args_to_skip;
};

enum cgraph_simd_clone_arg_type
{
  SIMD_CLONE_ARG_TYPE_VECTOR,
  SIMD_CLONE_ARG_TYPE_UNIFORM,
  SIMD_CLONE_ARG_TYPE_LINEAR_CONSTANT_STEP,
  SIMD_CLONE_ARG_TYPE_LINEAR_VARIABLE_STEP,
  SIMD_CLONE_ARG_TYPE_MASK
};

/* Function arguments in the original function of a SIMD clone.
   Supplementary data for `struct simd_clone'.  */

struct GTY(()) cgraph_simd_clone_arg {
  /* Original function argument as it originally existed in
     DECL_ARGUMENTS.  */
  tree orig_arg;

  /* orig_arg's function (or for extern functions type from
     TYPE_ARG_TYPES).  */
  tree orig_type;

  /* If argument is a vector, this holds the vector version of
     orig_arg that after adjusting the argument types will live in
     DECL_ARGUMENTS.  Otherwise, this is NULL.

     This basically holds:
       vector(simdlen) __typeof__(orig_arg) new_arg.  */
  tree vector_arg;

  /* vector_arg's type (or for extern functions new vector type.  */
  tree vector_type;

  /* If argument is a vector, this holds the array where the simd
     argument is held while executing the simd clone function.  This
     is a local variable in the cloned function.  Its content is
     copied from vector_arg upon entry to the clone.

     This basically holds:
       __typeof__(orig_arg) simd_array[simdlen].  */
  tree simd_array;

  /* A SIMD clone's argument can be either linear (constant or
     variable), uniform, or vector.  */
  enum cgraph_simd_clone_arg_type arg_type;

  /* For arg_type SIMD_CLONE_ARG_TYPE_LINEAR_CONSTANT_STEP this is
     the constant linear step, if arg_type is
     SIMD_CLONE_ARG_TYPE_LINEAR_VARIABLE_STEP, this is index of
     the uniform argument holding the step, otherwise 0.  */
  HOST_WIDE_INT linear_step;

  /* Variable alignment if available, otherwise 0.  */
  unsigned int alignment;
};

/* Specific data for a SIMD function clone.  */

struct GTY(()) cgraph_simd_clone {
  /* Number of words in the SIMD lane associated with this clone.  */
  unsigned int simdlen;

  /* Number of annotated function arguments in `args'.  This is
     usually the number of named arguments in FNDECL.  */
  unsigned int nargs;

  /* Max hardware vector size in bits for integral vectors.  */
  unsigned int vecsize_int;

  /* Max hardware vector size in bits for floating point vectors.  */
  unsigned int vecsize_float;

  /* The mangling character for a given vector size.  This is is used
     to determine the ISA mangling bit as specified in the Intel
     Vector ABI.  */
  unsigned char vecsize_mangle;

  /* True if this is the masked, in-branch version of the clone,
     otherwise false.  */
  unsigned int inbranch : 1;

  /* True if this is a Cilk Plus variant.  */
  unsigned int cilk_elemental : 1;

  /* Doubly linked list of SIMD clones.  */
  cgraph_node *prev_clone, *next_clone;

  /* Original cgraph node the SIMD clones were created for.  */
  cgraph_node *origin;

  /* Annotated function arguments for the original function.  */
  struct cgraph_simd_clone_arg GTY((length ("%h.nargs"))) args[1];
};

/* Function Multiversioning info.  */
struct GTY(()) cgraph_function_version_info {
  /* The cgraph_node for which the function version info is stored.  */
  cgraph_node *this_node;
  /* Chains all the semantically identical function versions.  The
     first function in this chain is the version_info node of the
     default function.  */
  struct cgraph_function_version_info *prev;
  /* If this version node corresponds to a dispatcher for function
     versions, this points to the version info node of the default
     function, the first node in the chain.  */
  struct cgraph_function_version_info *next;
  /* If this node corresponds to a function version, this points
     to the dispatcher function decl, which is the function that must
     be called to execute the right function version at run-time.

     If this cgraph node is a dispatcher (if dispatcher_function is
     true, in the cgraph_node struct) for function versions, this
     points to resolver function, which holds the function body of the
     dispatcher. The dispatcher decl is an alias to the resolver
     function decl.  */
  tree dispatcher_resolver;
};

#define DEFCIFCODE(code, type, string)	CIF_ ## code,
/* Reasons for inlining failures.  */

enum cgraph_inline_failed_t {
#include "cif-code.def"
  CIF_N_REASONS
};

enum cgraph_inline_failed_type_t
{
  CIF_FINAL_NORMAL = 0,
  CIF_FINAL_ERROR
};

struct cgraph_edge;

/* The cgraph data structure.
   Each function decl has assigned cgraph_node listing callees and callers.  */

struct GTY((tag ("SYMTAB_FUNCTION"))) cgraph_node : public symtab_node {
public:
  /* Remove the node from cgraph and all inline clones inlined into it.
     Skip however removal of FORBIDDEN_NODE and return true if it needs to be
     removed.  This allows to call the function from outer loop walking clone
     tree.  */
  bool remove_symbol_and_inline_clones (cgraph_node *forbidden_node = NULL);

  /* Record all references from cgraph_node that are taken
     in statement STMT.  */
  void record_stmt_references (gimple stmt);

  /* Like cgraph_set_call_stmt but walk the clone tree and update all
     clones sharing the same function body.
     When WHOLE_SPECULATIVE_EDGES is true, all three components of
     speculative edge gets updated.  Otherwise we update only direct
     call.  */
  void set_call_stmt_including_clones (gimple old_stmt, gimple new_stmt,
				       bool update_speculative = true);

  /* Walk the alias chain to return the function cgraph_node is alias of.
     Walk through thunk, too.
     When AVAILABILITY is non-NULL, get minimal availability in the chain.  */
  cgraph_node *function_symbol (enum availability *avail = NULL);

  /* Create node representing clone of N executed COUNT times.  Decrease
     the execution counts from original node too.
     The new clone will have decl set to DECL that may or may not be the same
     as decl of N.

     When UPDATE_ORIGINAL is true, the counts are subtracted from the original
     function's profile to reflect the fact that part of execution is handled
     by node.
     When CALL_DUPLICATOIN_HOOK is true, the ipa passes are acknowledged about
     the new clone. Otherwise the caller is responsible for doing so later.

     If the new node is being inlined into another one, NEW_INLINED_TO should be
     the outline function the new one is (even indirectly) inlined to.
     All hooks will see this in node's global.inlined_to, when invoked.
     Can be NULL if the node is not inlined.  */
  cgraph_node *create_clone (tree decl, gcov_type count, int freq,
			     bool update_original,
			     vec<cgraph_edge *> redirect_callers,
			     bool call_duplication_hook,
			     struct cgraph_node *new_inlined_to,
			     bitmap args_to_skip);

  /* Create callgraph node clone with new declaration.  The actual body will
     be copied later at compilation stage.  */
  cgraph_node *create_virtual_clone (vec<cgraph_edge *> redirect_callers,
				     vec<ipa_replace_map *, va_gc> *tree_map,
				     bitmap args_to_skip, const char * suffix);

  /* cgraph node being removed from symbol table; see if its entry can be
   replaced by other inline clone.  */
  cgraph_node *find_replacement (void);

  /* Create a new cgraph node which is the new version of
     callgraph node.  REDIRECT_CALLERS holds the callers
     edges which should be redirected to point to
     NEW_VERSION.  ALL the callees edges of the node
     are cloned to the new version node.  Return the new
     version node.

     If non-NULL BLOCK_TO_COPY determine what basic blocks
     was copied to prevent duplications of calls that are dead
     in the clone.  */

  cgraph_node *create_version_clone (tree new_decl,
				    vec<cgraph_edge *> redirect_callers,
				    bitmap bbs_to_copy);

  /* Perform function versioning.
     Function versioning includes copying of the tree and
     a callgraph update (creating a new cgraph node and updating
     its callees and callers).

     REDIRECT_CALLERS varray includes the edges to be redirected
     to the new version.

     TREE_MAP is a mapping of tree nodes we want to replace with
     new ones (according to results of prior analysis).

     If non-NULL ARGS_TO_SKIP determine function parameters to remove
     from new version.
     If SKIP_RETURN is true, the new version will return void.
     If non-NULL BLOCK_TO_COPY determine what basic blocks to copy.
     If non_NULL NEW_ENTRY determine new entry BB of the clone.

     Return the new version's cgraph node.  */
  cgraph_node *create_version_clone_with_body
    (vec<cgraph_edge *> redirect_callers,
     vec<ipa_replace_map *, va_gc> *tree_map, bitmap args_to_skip,
     bool skip_return, bitmap bbs_to_copy, basic_block new_entry_block,
     const char *clone_name);

  /* Insert a new cgraph_function_version_info node into cgraph_fnver_htab
     corresponding to cgraph_node.  */
  struct cgraph_function_version_info *insert_new_function_version (void);

  /* Get the cgraph_function_version_info node corresponding to node.  */
  struct cgraph_function_version_info *function_version (void);

  /* Discover all functions and variables that are trivially needed, analyze
     them as well as all functions and variables referred by them  */
  void analyze (void);

  /* Add thunk alias into callgraph.  The alias declaration is ALIAS and it
     aliases DECL with an adjustments made into the first parameter.
     See comments in thunk_adjust for detail on the parameters.  */
  cgraph_node * create_thunk (tree alias, tree, bool this_adjusting,
			      HOST_WIDE_INT fixed_offset,
			      HOST_WIDE_INT virtual_value,
			      tree virtual_offset,
			      tree real_alias);


  /* Return node that alias is aliasing.  */
  inline cgraph_node *get_alias_target (void);

  /* Given function symbol, walk the alias chain to return the function node
     is alias of. Do not walk through thunks.
     When AVAILABILITY is non-NULL, get minimal availability in the chain.  */

  cgraph_node *ultimate_alias_target (availability *availability = NULL);

  /* Expand thunk NODE to gimple if possible.
     When FORCE_GIMPLE_THUNK is true, gimple thunk is created and
     no assembler is produced.
     When OUTPUT_ASM_THUNK is true, also produce assembler for
     thunks that are not lowered.  */
  bool expand_thunk (bool output_asm_thunks, bool force_gimple_thunk);

  /* As an GCC extension we allow redefinition of the function.  The
     semantics when both copies of bodies differ is not well defined.
     We replace the old body with new body so in unit at a time mode
     we always use new body, while in normal mode we may end up with
     old body inlined into some functions and new body expanded and
     inlined in others.  */
  void reset (void);

  /* Creates a wrapper from cgraph_node to TARGET node. Thunk is used for this
     kind of wrapper method.  */
  void create_wrapper (cgraph_node *target);

  /* Verify cgraph nodes of the cgraph node.  */
  void DEBUG_FUNCTION verify_node (void);

  /* Remove function from symbol table.  */
  void remove (void);

  /* Dump call graph node to file F.  */
  void dump (FILE *f);

  /* Dump call graph node to stderr.  */
  void DEBUG_FUNCTION debug (void);

  /* When doing LTO, read cgraph_node's body from disk if it is not already
     present.  */
  bool get_body (void);

  /* Release memory used to represent body of function.
     Use this only for functions that are released before being translated to
     target code (i.e. RTL).  Functions that are compiled to RTL and beyond
     are free'd in final.c via free_after_compilation().  */
  void release_body (void);

  /* cgraph_node is no longer nested function; update cgraph accordingly.  */
  void unnest (void);

  /* Bring cgraph node local.  */
  void make_local (void);

  /* Likewise indicate that a node is having address taken.  */
  void mark_address_taken (void);

  /* Set fialization priority to PRIORITY.  */
  void set_fini_priority (priority_type priority);

  /* Return the finalization priority.  */
  priority_type get_fini_priority (void);

  /* Create edge from a given function to CALLEE in the cgraph.  */
  struct cgraph_edge *create_edge (cgraph_node *callee,
				   gimple call_stmt, gcov_type count,
				   int freq);
  /* Create an indirect edge with a yet-undetermined callee where the call
     statement destination is a formal parameter of the caller with index
     PARAM_INDEX. */
  struct cgraph_edge *create_indirect_edge (gimple call_stmt, int ecf_flags,
					    gcov_type count, int freq,
					    bool compute_indirect_info = true);

  /* Like cgraph_create_edge walk the clone tree and update all clones sharing
   same function body.  If clones already have edge for OLD_STMT; only
   update the edge same way as cgraph_set_call_stmt_including_clones does.  */
  void create_edge_including_clones (struct cgraph_node *callee,
				     gimple old_stmt, gimple stmt,
				     gcov_type count,
				     int freq,
				     cgraph_inline_failed_t reason);

  /* Return the callgraph edge representing the GIMPLE_CALL statement
     CALL_STMT.  */
  cgraph_edge *get_edge (gimple call_stmt);

  /* Collect all callers of cgraph_node and its aliases that are known to lead
     to NODE (i.e. are not overwritable).  */
  vec<cgraph_edge *> collect_callers (void);

  /* Remove all callers from the node.  */
  void remove_callers (void);

  /* Remove all callees from the node.  */
  void remove_callees (void);

  /* Return function availability.  See cgraph.h for description of individual
     return values.  */
  enum availability get_availability (void);

  /* Set TREE_NOTHROW on cgraph_node's decl and on aliases of the node
     if any to NOTHROW.  */
  void set_nothrow_flag (bool nothrow);

  /* Set TREE_READONLY on cgraph_node's decl and on aliases of the node
     if any to READONLY.  */
  void set_const_flag (bool readonly, bool looping);

  /* Set DECL_PURE_P on cgraph_node's decl and on aliases of the node
     if any to PURE.  */
  void set_pure_flag (bool pure, bool looping);

  /* Call all node duplication hooks.  */
  void call_duplication_hooks (cgraph_node *node2);

  /* Call calback on function and aliases associated to the function.
     When INCLUDE_OVERWRITABLE is false, overwritable aliases and thunks are
     skipped. */

  bool call_for_symbol_and_aliases (bool (*callback) (cgraph_node *,
						      void *),
				    void *data, bool include_overwritable);

  /* Call calback on cgraph_node, thunks and aliases associated to NODE.
     When INCLUDE_OVERWRITABLE is false, overwritable aliases and thunks are
     skipped.  */
  bool call_for_symbol_thunks_and_aliases (bool (*callback) (cgraph_node *node,
							   void *data),
					 void *data,
					 bool include_overwritable);

  /* Call all node insertion hooks.  */
  void call_function_insertion_hooks (void);

  /* Likewise indicate that a node is needed, i.e. reachable via some
     external means.  */
  inline void mark_force_output (void);

  /* Return true when function can be marked local.  */
  bool local_p (void);

  /* Return true if cgraph_node can be made local for API change.
     Extern inline functions and C++ COMDAT functions can be made local
     at the expense of possible code size growth if function is used in multiple
     compilation units.  */
  bool can_be_local_p (void);

  /* Return true when cgraph_node can not return or throw and thus
     it is safe to ignore its side effects for IPA analysis.  */
  bool cannot_return_p (void);

  /* Return true when function cgraph_node and all its aliases are only called
     directly.
     i.e. it is not externally visible, address was not taken and
     it is not used in any other non-standard way.  */
  bool only_called_directly_p (void);

  /* Return true when function is only called directly or it has alias.
     i.e. it is not externally visible, address was not taken and
     it is not used in any other non-standard way.  */
  inline bool only_called_directly_or_aliased_p (void);

  /* Return true when function cgraph_node can be expected to be removed
     from program when direct calls in this compilation unit are removed.

     As a special case COMDAT functions are
     cgraph_can_remove_if_no_direct_calls_p while the are not
     cgraph_only_called_directly_p (it is possible they are called from other
     unit)

     This function behaves as cgraph_only_called_directly_p because eliminating
     all uses of COMDAT function does not make it necessarily disappear from
     the program unless we are compiling whole program or we do LTO.  In this
     case we know we win since dynamic linking will not really discard the
     linkonce section.  */
  bool will_be_removed_from_program_if_no_direct_calls_p (void);

  /* Return true when function can be removed from callgraph
     if all direct calls are eliminated.  */
  bool can_remove_if_no_direct_calls_and_refs_p (void);

  /* Return true when function cgraph_node and its aliases can be removed from
     callgraph if all direct calls are eliminated.  */
  bool can_remove_if_no_direct_calls_p (void);

  /* Return true when callgraph node is a function with Gimple body defined
     in current unit.  Functions can also be define externally or they
     can be thunks with no Gimple representation.

     Note that at WPA stage, the function body may not be present in memory.  */
  inline bool has_gimple_body_p (void);

  /* Return true if function should be optimized for size.  */
  bool optimize_for_size_p (void);

  /* Dump the callgraph to file F.  */
  static void dump_cgraph (FILE *f);

  /* Dump the call graph to stderr.  */
  static inline void debug_cgraph (void)
  {
    dump_cgraph (stderr);
  }

  /* Record that DECL1 and DECL2 are semantically identical function
     versions.  */
  static void record_function_versions (tree decl1, tree decl2);

  /* Remove the cgraph_function_version_info and cgraph_node for DECL.  This
     DECL is a duplicate declaration.  */
  static void delete_function_version (tree decl);

  /* Add the function FNDECL to the call graph.
     Unlike cgraph_finalize_function, this function is intended to be used
     by middle end and allows insertion of new function at arbitrary point
     of compilation.  The function can be either in high, low or SSA form
     GIMPLE.

     The function is assumed to be reachable and have address taken (so no
     API breaking optimizations are performed on it).

     Main work done by this function is to enqueue the function for later
     processing to avoid need the passes to be re-entrant.  */
  static void add_new_function (tree fndecl, bool lowered);

  /* Return callgraph node for given symbol and check it is a function. */
  static inline cgraph_node *get (const_tree decl)
  {
    gcc_checking_assert (TREE_CODE (decl) == FUNCTION_DECL);
    return dyn_cast <cgraph_node *> (symtab_node::get (decl));
  }

  /* Return cgraph node assigned to DECL.  Create new one when needed.  */
  static cgraph_node * create (tree decl);

  /* Allocate new callgraph node and insert it into basic data structures.  */
  static cgraph_node * create_empty (void);

  /* Try to find a call graph node for declaration DECL and if it does not
     exist or if it corresponds to an inline clone, create a new one.  */
  static cgraph_node * get_create (tree);

  /* Return the cgraph node that has ASMNAME for its DECL_ASSEMBLER_NAME.
     Return NULL if there's no such node.  */
  static cgraph_node *get_for_asmname (tree asmname);

  /* Attempt to mark ALIAS as an alias to DECL.  Return alias node if
     successful and NULL otherwise.
     Same body aliases are output whenever the body of DECL is output,
     and cgraph_node::get (ALIAS) transparently
     returns cgraph_node::get (DECL).  */
  static cgraph_node * create_same_body_alias (tree alias, tree decl);

  /* Worker for cgraph_can_remove_if_no_direct_calls_p.  */
  static bool used_from_object_file_p_worker (cgraph_node *node, void *)
  {
    return node->used_from_object_file_p ();
  }

  /* Return true when cgraph_node can not be local.
     Worker for cgraph_local_node_p.  */
  static bool non_local_p (cgraph_node *node, void *);

  /* Verify whole cgraph structure.  */
  static void DEBUG_FUNCTION verify_cgraph_nodes (void);

  /* Worker to bring NODE local.  */
  static bool make_local (cgraph_node *node, void *);

  /* Mark ALIAS as an alias to DECL.  DECL_NODE is cgraph node representing
     the function body is associated
     with (not necessarily cgraph_node (DECL).  */
  static cgraph_node *create_alias (tree alias, tree target);

  static cgraph_edge * create_edge (cgraph_node *caller, cgraph_node *callee,
				    gimple call_stmt, gcov_type count,
				    int freq,
				    bool indir_unknown_callee);

  struct cgraph_edge *callees;
  struct cgraph_edge *callers;
  /* List of edges representing indirect calls with a yet undetermined
     callee.  */
  struct cgraph_edge *indirect_calls;
  /* For nested functions points to function the node is nested in.  */
  cgraph_node *origin;
  /* Points to first nested function, if any.  */
  cgraph_node *nested;
  /* Pointer to the next function with same origin, if any.  */
  cgraph_node *next_nested;
  /* Pointer to the next clone.  */
  cgraph_node *next_sibling_clone;
  cgraph_node *prev_sibling_clone;
  cgraph_node *clones;
  cgraph_node *clone_of;
  /* For functions with many calls sites it holds map from call expression
     to the edge to speed up cgraph_edge function.  */
  htab_t GTY((param_is (struct cgraph_edge))) call_site_hash;
  /* Declaration node used to be clone of. */
  tree former_clone_of;

  /* If this is a SIMD clone, this points to the SIMD specific
     information for it.  */
  struct cgraph_simd_clone *simdclone;
  /* If this function has SIMD clones, this points to the first clone.  */
  cgraph_node *simd_clones;

  /* Interprocedural passes scheduled to have their transform functions
     applied next time we execute local pass on them.  We maintain it
     per-function in order to allow IPA passes to introduce new functions.  */
  vec<ipa_opt_pass> GTY((skip)) ipa_transforms_to_apply;

  struct cgraph_local_info local;
  struct cgraph_global_info global;
  struct cgraph_rtl_info rtl;
  struct cgraph_clone_info clone;
  struct cgraph_thunk_info thunk;

  /* Expected number of executions: calculated in profile.c.  */
  gcov_type count;
  /* How to scale counts at materialization time; used to merge
     LTO units with different number of profile runs.  */
  int count_materialization_scale;
  /* Unique id of the node.  */
  int uid;
  /* ID assigned by the profiling.  */
  unsigned int profile_id;
  /* Time profiler: first run of function.  */
  int tp_first_run;

  /* Set when decl is an abstract function pointed to by the
     ABSTRACT_DECL_ORIGIN of a reachable function.  */
  unsigned used_as_abstract_origin : 1;
  /* Set once the function is lowered (i.e. its CFG is built).  */
  unsigned lowered : 1;
  /* Set once the function has been instantiated and its callee
     lists created.  */
  unsigned process : 1;
  /* How commonly executed the node is.  Initialized during branch
     probabilities pass.  */
  ENUM_BITFIELD (node_frequency) frequency : 2;
  /* True when function can only be called at startup (from static ctor).  */
  unsigned only_called_at_startup : 1;
  /* True when function can only be called at startup (from static dtor).  */
  unsigned only_called_at_exit : 1;
  /* True when function is the transactional clone of a function which
     is called only from inside transactions.  */
  /* ?? We should be able to remove this.  We have enough bits in
     cgraph to calculate it.  */
  unsigned tm_clone : 1;
  /* True if this decl is a dispatcher for function versions.  */
  unsigned dispatcher_function : 1;
  /* True if this decl calls a COMDAT-local function.  This is set up in
     compute_inline_parameters and inline_call.  */
  unsigned calls_comdat_local : 1;
};

/* A cgraph node set is a collection of cgraph nodes.  A cgraph node
   can appear in multiple sets.  */
struct cgraph_node_set_def
{
  hash_map<cgraph_node *, size_t> *map;
  vec<cgraph_node *> nodes;
};

typedef cgraph_node_set_def *cgraph_node_set;
typedef struct varpool_node_set_def *varpool_node_set;

class varpool_node;

/* A varpool node set is a collection of varpool nodes.  A varpool node
   can appear in multiple sets.  */
struct varpool_node_set_def
{
  hash_map<varpool_node *, size_t> * map;
  vec<varpool_node *> nodes;
};

/* Iterator structure for cgraph node sets.  */
struct cgraph_node_set_iterator
{
  cgraph_node_set set;
  unsigned index;
};

/* Iterator structure for varpool node sets.  */
struct varpool_node_set_iterator
{
  varpool_node_set set;
  unsigned index;
};

/* Structure containing additional information about an indirect call.  */

struct GTY(()) cgraph_indirect_call_info
{
  /* When polymorphic is set, this field contains offset where the object which
     was actually used in the polymorphic resides within a larger structure.
     If agg_contents is set, the field contains the offset within the aggregate
     from which the address to call was loaded.  */
  HOST_WIDE_INT offset, speculative_offset;
  /* OBJ_TYPE_REF_TOKEN of a polymorphic call (if polymorphic is set).  */
  HOST_WIDE_INT otr_token;
  /* Type of the object from OBJ_TYPE_REF_OBJECT. */
  tree otr_type, outer_type, speculative_outer_type;
  /* Index of the parameter that is called.  */
  int param_index;
  /* ECF flags determined from the caller.  */
  int ecf_flags;
  /* Profile_id of common target obtrained from profile.  */
  int common_target_id;
  /* Probability that call will land in function with COMMON_TARGET_ID.  */
  int common_target_probability;

  /* Set when the call is a virtual call with the parameter being the
     associated object pointer rather than a simple direct call.  */
  unsigned polymorphic : 1;
  /* Set when the call is a call of a pointer loaded from contents of an
     aggregate at offset.  */
  unsigned agg_contents : 1;
  /* Set when this is a call through a member pointer.  */
  unsigned member_ptr : 1;
  /* When the previous bit is set, this one determines whether the destination
     is loaded from a parameter passed by reference. */
  unsigned by_ref : 1;
  unsigned int maybe_in_construction : 1;
  unsigned int maybe_derived_type : 1;
  unsigned int speculative_maybe_derived_type : 1;
};

struct GTY((chain_next ("%h.next_caller"), chain_prev ("%h.prev_caller"))) cgraph_edge {
  /* Expected number of executions: calculated in profile.c.  */
  gcov_type count;
  cgraph_node *caller;
  cgraph_node *callee;
  struct cgraph_edge *prev_caller;
  struct cgraph_edge *next_caller;
  struct cgraph_edge *prev_callee;
  struct cgraph_edge *next_callee;
  gimple call_stmt;
  /* Additional information about an indirect call.  Not cleared when an edge
     becomes direct.  */
  struct cgraph_indirect_call_info *indirect_info;
  PTR GTY ((skip (""))) aux;
  /* When equal to CIF_OK, inline this call.  Otherwise, points to the
     explanation why function was not inlined.  */
  enum cgraph_inline_failed_t inline_failed;
  /* The stmt_uid of call_stmt.  This is used by LTO to recover the call_stmt
     when the function is serialized in.  */
  unsigned int lto_stmt_uid;
  /* Expected frequency of executions within the function.
     When set to CGRAPH_FREQ_BASE, the edge is expected to be called once
     per function call.  The range is 0 to CGRAPH_FREQ_MAX.  */
  int frequency;
  /* Unique id of the edge.  */
  int uid;
  /* Whether this edge was made direct by indirect inlining.  */
  unsigned int indirect_inlining_edge : 1;
  /* Whether this edge describes an indirect call with an undetermined
     callee.  */
  unsigned int indirect_unknown_callee : 1;
  /* Whether this edge is still a dangling  */
  /* True if the corresponding CALL stmt cannot be inlined.  */
  unsigned int call_stmt_cannot_inline_p : 1;
  /* Can this call throw externally?  */
  unsigned int can_throw_external : 1;
  /* Edges with SPECULATIVE flag represents indirect calls that was
     speculatively turned into direct (i.e. by profile feedback).
     The final code sequence will have form:

     if (call_target == expected_fn)
       expected_fn ();
     else
       call_target ();

     Every speculative call is represented by three components attached
     to a same call statement:
     1) a direct call (to expected_fn)
     2) an indirect call (to call_target)
     3) a IPA_REF_ADDR refrence to expected_fn.

     Optimizers may later redirect direct call to clone, so 1) and 3)
     do not need to necesarily agree with destination.  */
  unsigned int speculative : 1;
};

#define CGRAPH_FREQ_BASE 1000
#define CGRAPH_FREQ_MAX 100000

/* The varpool data structure.
   Each static variable decl has assigned varpool_node.  */

class GTY((tag ("SYMTAB_VARIABLE"))) varpool_node : public symtab_node {
public:
  /* Dump given varpool node to F.  */
  void dump (FILE *f);

  /* Dump given varpool node to stderr.  */
  void DEBUG_FUNCTION debug (void);

  /* Remove variable from symbol table.  */
  void remove (void);

  /* Remove node initializer when it is no longer needed.  */
  void remove_initializer (void);

  void analyze (void);

  /* Return variable availability.  */
  availability get_availability (void);

  /* When doing LTO, read variable's constructor from disk if
     it is not already present.  */
  tree get_constructor (void);

  /* Return true if variable has constructor that can be used for folding.  */
  bool ctor_useable_for_folding_p (void);

  /* For given variable pool node, walk the alias chain to return the function
     the variable is alias of. Do not walk through thunks.
     When AVAILABILITY is non-NULL, get minimal availability in the chain.  */
  inline varpool_node *ultimate_alias_target
    (availability *availability = NULL);

  /* Return node that alias is aliasing.  */
  inline varpool_node *get_alias_target (void);

  /* Output one variable, if necessary.  Return whether we output it.  */
  bool assemble_decl (void);

  /* For variables in named sections make sure get_variable_section
     is called before we switch to those sections.  Then section
     conflicts between read-only and read-only requiring relocations
     sections can be resolved.  */
  void finalize_named_section_flags (void);

  /* Call calback on varpool symbol and aliases associated to varpool symbol.
     When INCLUDE_OVERWRITABLE is false, overwritable aliases and thunks are
     skipped. */
  bool call_for_node_and_aliases (bool (*callback) (varpool_node *, void *),
				  void *data,
				   bool include_overwritable);

  /* Return true when variable should be considered externally visible.  */
  bool externally_visible_p (void);

  /* Return true when all references to variable must be visible
     in ipa_ref_list.
     i.e. if the variable is not externally visible or not used in some magic
     way (asm statement or such).
     The magic uses are all summarized in force_output flag.  */
  inline bool all_refs_explicit_p ();

  /* Return true when variable can be removed from variable pool
     if all direct calls are eliminated.  */
  inline bool can_remove_if_no_refs_p (void);

  /* Return varpool node for given symbol and check it is a function. */
  static inline varpool_node *get (const_tree decl);

  /* Mark DECL as finalized.  By finalizing the declaration, frontend instruct
     the middle end to output the variable to asm file, if needed or externally
     visible.  */
  static void finalize_decl (tree decl);

  /* Output all variables enqueued to be assembled.  */
  static bool output_variables (void);

  /* Attempt to mark ALIAS as an alias to DECL.  Return TRUE if successful.
     Extra name aliases are output whenever DECL is output.  */
  static varpool_node * create_extra_name_alias (tree alias, tree decl);

  /* Attempt to mark ALIAS as an alias to DECL.  Return TRUE if successful.
     Extra name aliases are output whenever DECL is output.  */
  static varpool_node * create_alias (tree, tree);

  /* Dump the variable pool to F.  */
  static void dump_varpool (FILE *f);

  /* Dump the variable pool to stderr.  */
  static void DEBUG_FUNCTION debug_varpool (void);

  /* Allocate new callgraph node and insert it into basic data structures.  */
  static varpool_node *create_empty (void);

  /* Return varpool node assigned to DECL.  Create new one when needed.  */
  static varpool_node *get_create (tree decl);

  /* Given an assembler name, lookup node.  */
  static varpool_node *get_for_asmname (tree asmname);

  /* Set when variable is scheduled to be assembled.  */
  unsigned output : 1;

  /* Set if the variable is dynamically initialized, except for
     function local statics.   */
  unsigned dynamically_initialized : 1;

  ENUM_BITFIELD(tls_model) tls_model : 3;

  /* Set if the variable is known to be used by single function only.
     This is computed by ipa_signle_use pass and used by late optimizations
     in places where optimization would be valid for local static variable
     if we did not do any inter-procedural code movement.  */
  unsigned used_by_single_function : 1;

private:
  /* Assemble thunks and aliases associated to varpool node.  */
  void assemble_aliases (void);
};

/* Every top level asm statement is put into a asm_node.  */

struct GTY(()) asm_node {
  /* Next asm node.  */
  struct asm_node *next;
  /* String for this asm node.  */
  tree asm_str;
  /* Ordering of all cgraph nodes.  */
  int order;
};

/* Report whether or not THIS symtab node is a function, aka cgraph_node.  */

template <>
template <>
inline bool
is_a_helper <cgraph_node *>::test (symtab_node *p)
{
  return p && p->type == SYMTAB_FUNCTION;
}

/* Report whether or not THIS symtab node is a vriable, aka varpool_node.  */

template <>
template <>
inline bool
is_a_helper <varpool_node *>::test (symtab_node *p)
{
  return p && p->type == SYMTAB_VARIABLE;
}

extern GTY(()) symtab_node *symtab_nodes;
extern GTY(()) int cgraph_n_nodes;
extern GTY(()) int cgraph_max_uid;
extern GTY(()) int cgraph_edge_max_uid;
extern bool cgraph_global_info_ready;
enum cgraph_state
{
  /* Frontend is parsing and finalizing functions.  */
  CGRAPH_STATE_PARSING,
  /* Callgraph is being constructed.  It is safe to add new functions.  */
  CGRAPH_STATE_CONSTRUCTION,
  /* Callgraph is being at LTO time.  */
  CGRAPH_LTO_STREAMING,
  /* Callgraph is built and IPA passes are being run.  */
  CGRAPH_STATE_IPA,
  /* Callgraph is built and all functions are transformed to SSA form.  */
  CGRAPH_STATE_IPA_SSA,
  /* Functions are now ordered and being passed to RTL expanders.  */
  CGRAPH_STATE_EXPANSION,
  /* All cgraph expansion is done.  */
  CGRAPH_STATE_FINISHED
};
extern enum cgraph_state cgraph_state;
extern bool cgraph_function_flags_ready;
extern vec<cgraph_node *> cgraph_new_nodes;

extern GTY(()) struct asm_node *asm_nodes;
extern GTY(()) int symtab_order;
extern bool cpp_implicit_aliases_done;

/* In symtab.c  */
symtab_node *symtab_node_for_asm (const_tree asmname);

/* In cgraph.c  */
void release_function_body (tree);
struct cgraph_indirect_call_info *cgraph_allocate_init_indirect_info (void);
void cgraph_remove_edge (struct cgraph_edge *);

void cgraph_set_call_stmt (struct cgraph_edge *, gimple, bool update_speculative = true);
void cgraph_update_edges_for_call_stmt (gimple, tree, gimple);
struct cgraph_local_info *cgraph_local_info (tree);
struct cgraph_global_info *cgraph_global_info (tree);
struct cgraph_rtl_info *cgraph_rtl_info (tree);
void cgraph_call_edge_duplication_hooks (struct cgraph_edge *,
				         struct cgraph_edge *);

bool cgraph_function_possibly_inlined_p (tree);
bool cgraph_edge_cannot_lead_to_return (struct cgraph_edge *);
void cgraph_redirect_edge_callee (struct cgraph_edge *, cgraph_node *);
struct cgraph_edge *cgraph_make_edge_direct (struct cgraph_edge *,
					     cgraph_node *);

const char* cgraph_inline_failed_string (cgraph_inline_failed_t);
cgraph_inline_failed_type_t cgraph_inline_failed_type (cgraph_inline_failed_t);

bool resolution_used_from_other_file_p (enum ld_plugin_symbol_resolution);
typedef void (*cgraph_edge_hook)(struct cgraph_edge *, void *);
typedef void (*cgraph_node_hook)(cgraph_node *, void *);
typedef void (*varpool_node_hook)(varpool_node *, void *);
typedef void (*cgraph_2edge_hook)(struct cgraph_edge *, struct cgraph_edge *,
				  void *);
typedef void (*cgraph_2node_hook)(cgraph_node *, cgraph_node *,
				  void *);
struct cgraph_edge_hook_list;
struct cgraph_node_hook_list;
struct varpool_node_hook_list;
struct cgraph_2edge_hook_list;
struct cgraph_2node_hook_list;
struct cgraph_edge_hook_list *cgraph_add_edge_removal_hook (cgraph_edge_hook, void *);
void cgraph_remove_edge_removal_hook (struct cgraph_edge_hook_list *);
cgraph_node_hook_list *cgraph_add_node_removal_hook (cgraph_node_hook,
							    void *);
void cgraph_remove_node_removal_hook (cgraph_node_hook_list *);
struct varpool_node_hook_list *varpool_add_node_removal_hook (varpool_node_hook,
							      void *);
void varpool_remove_node_removal_hook (struct varpool_node_hook_list *);
cgraph_node_hook_list *cgraph_add_function_insertion_hook (cgraph_node_hook,
							          void *);
void cgraph_remove_function_insertion_hook (cgraph_node_hook_list *);
struct varpool_node_hook_list *varpool_add_variable_insertion_hook (varpool_node_hook,
							            void *);
void varpool_remove_variable_insertion_hook (struct varpool_node_hook_list *);
struct cgraph_2edge_hook_list *cgraph_add_edge_duplication_hook (cgraph_2edge_hook, void *);
void cgraph_remove_edge_duplication_hook (struct cgraph_2edge_hook_list *);
struct cgraph_2node_hook_list *cgraph_add_node_duplication_hook (cgraph_2node_hook, void *);
void cgraph_remove_node_duplication_hook (struct cgraph_2node_hook_list *);
gimple cgraph_redirect_edge_call_stmt_to_callee (struct cgraph_edge *);
struct cgraph_edge *
cgraph_turn_edge_to_speculative (struct cgraph_edge *,
				 cgraph_node *,
				 gcov_type, int);
void cgraph_speculative_call_info (struct cgraph_edge *,
				   struct cgraph_edge *&,
				   struct cgraph_edge *&,
				   struct ipa_ref *&);
extern bool gimple_check_call_matching_types (gimple, tree, bool);

/* In cgraphunit.c  */
struct asm_node *add_asm_node (tree);
extern FILE *cgraph_dump_file;
void cgraph_finalize_function (tree, bool);
void finalize_compilation_unit (void);
void compile (void);
void init_cgraph (void);
void cgraph_process_new_functions (void);
void cgraph_process_same_body_aliases (void);
/*  Initialize datastructures so DECL is a function in lowered gimple form.
    IN_SSA is true if the gimple is in SSA.  */
basic_block init_lowered_empty_function (tree, bool);

/* In cgraphclones.c  */

struct cgraph_edge * cgraph_clone_edge (struct cgraph_edge *,
					cgraph_node *, gimple,
					unsigned, gcov_type, int, bool);
tree clone_function_name (tree decl, const char *);

void cgraph_materialize_all_clones (void);
void tree_function_versioning (tree, tree, vec<ipa_replace_map *, va_gc> *,
			       bool, bitmap, bool, bitmap, basic_block);
struct cgraph_edge *cgraph_resolve_speculation (struct cgraph_edge *, tree);

/* In cgraphbuild.c  */
unsigned int rebuild_cgraph_edges (void);
void cgraph_rebuild_references (void);
int compute_call_stmt_bb_frequency (tree, basic_block bb);
void record_references_in_initializer (tree, bool);

/* In ipa.c  */
bool symtab_remove_unreachable_nodes (bool, FILE *);
void cgraph_build_static_cdtor (char which, tree body, int priority);
void ipa_discover_readonly_nonaddressable_vars (void);

/* In predict.c  */
bool cgraph_maybe_hot_edge_p (struct cgraph_edge *e);

/* In varpool.c  */
void varpool_reset_queue (void);
tree ctor_for_folding (tree);
void varpool_add_new_variable (tree);
void symtab_initialize_asm_name_hash (void);
void symtab_prevail_in_asm_name_hash (symtab_node *node);

/* In cgraph.c */
extern void change_decl_assembler_name (tree, tree);

/* Return true when the symbol is real symbol, i.e. it is not inline clone
   or abstract function kept for debug info purposes only.  */
inline bool
symtab_node::real_symbol_p (void)
{
  cgraph_node *cnode;

  if (DECL_ABSTRACT (decl))
    return false;
  if (!is_a <cgraph_node *> (this))
    return true;
  cnode = dyn_cast <cgraph_node *> (this);
  if (cnode->global.inlined_to)
    return false;
  return true;
}

/* Return true if DECL should have entry in symbol table if used.
   Those are functions and static & external veriables*/

static inline bool
decl_in_symtab_p (const_tree decl)
{
  return (TREE_CODE (decl) == FUNCTION_DECL
          || (TREE_CODE (decl) == VAR_DECL
	      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl))));
}

inline bool
symtab_node::in_same_comdat_group_p (symtab_node *target)
{
  symtab_node *source = this;

  if (cgraph_node *cn = dyn_cast <cgraph_node *> (target))
    {
      if (cn->global.inlined_to)
	source = cn->global.inlined_to;
    }
  if (cgraph_node *cn = dyn_cast <cgraph_node *> (target))
    {
      if (cn->global.inlined_to)
	target = cn->global.inlined_to;
    }

  return source->get_comdat_group () == target->get_comdat_group ();
}

/* Return node that alias is aliasing.  */

inline symtab_node *
symtab_node::get_alias_target (void)
{
  struct ipa_ref *ref = NULL;
  iterate_reference (0, ref);
  gcc_checking_assert (ref->use == IPA_REF_ALIAS);
  return ref->referred;
}

/* Return next reachable static symbol with initializer after the node.  */
inline symtab_node *
symtab_node::next_defined_symbol (void)
{
  symtab_node *node1 = next;

  for (; node1; node1 = node1->next)
    if (node1->definition)
      return node1;

  return NULL;
}

/* Return varpool node for given symbol and check it is a function. */
inline varpool_node *
varpool_node::get (const_tree decl)
{
  gcc_checking_assert (TREE_CODE (decl) == VAR_DECL);
  return dyn_cast<varpool_node *> (symtab_node::get (decl));
}

/* Walk all symbols.  */
#define FOR_EACH_SYMBOL(node) \
   for ((node) = symtab_nodes; (node); (node) = (node)->next)

/* Return first static symbol with definition.  */
static inline symtab_node *
symtab_first_defined_symbol (void)
{
  symtab_node *node;

  for (node = symtab_nodes; node; node = node->next)
    if (node->definition)
      return node;

  return NULL;
}

/* Walk all symbols with definitions in current unit.  */
#define FOR_EACH_DEFINED_SYMBOL(node) \
   for ((node) = symtab_first_defined_symbol (); (node); \
	(node) = node->next_defined_symbol ())

/* Return first variable.  */
static inline varpool_node *
varpool_first_variable (void)
{
  symtab_node *node;
  for (node = symtab_nodes; node; node = node->next)
    if (varpool_node *vnode = dyn_cast <varpool_node *> (node))
      return vnode;
  return NULL;
}

/* Return next variable after NODE.  */
static inline varpool_node *
varpool_next_variable (varpool_node *node)
{
  symtab_node *node1 = node->next;
  for (; node1; node1 = node1->next)
    if (varpool_node *vnode1 = dyn_cast <varpool_node *> (node1))
      return vnode1;
  return NULL;
}
/* Walk all variables.  */
#define FOR_EACH_VARIABLE(node) \
   for ((node) = varpool_first_variable (); \
        (node); \
	(node) = varpool_next_variable ((node)))

/* Return first static variable with initializer.  */
static inline varpool_node *
varpool_first_static_initializer (void)
{
  symtab_node *node;
  for (node = symtab_nodes; node; node = node->next)
    {
      varpool_node *vnode = dyn_cast <varpool_node *> (node);
      if (vnode && DECL_INITIAL (node->decl))
	return vnode;
    }
  return NULL;
}

/* Return next static variable with initializer after NODE.  */
static inline varpool_node *
varpool_next_static_initializer (varpool_node *node)
{
  symtab_node *node1 = node->next;
  for (; node1; node1 = node1->next)
    {
      varpool_node *vnode1 = dyn_cast <varpool_node *> (node1);
      if (vnode1 && DECL_INITIAL (node1->decl))
	return vnode1;
    }
  return NULL;
}

/* Walk all static variables with initializer set.  */
#define FOR_EACH_STATIC_INITIALIZER(node) \
   for ((node) = varpool_first_static_initializer (); (node); \
        (node) = varpool_next_static_initializer (node))

/* Return first static variable with definition.  */
static inline varpool_node *
varpool_first_defined_variable (void)
{
  symtab_node *node;
  for (node = symtab_nodes; node; node = node->next)
    {
      varpool_node *vnode = dyn_cast <varpool_node *> (node);
      if (vnode && vnode->definition)
	return vnode;
    }
  return NULL;
}

/* Return next static variable with definition after NODE.  */
static inline varpool_node *
varpool_next_defined_variable (varpool_node *node)
{
  symtab_node *node1 = node->next;
  for (; node1; node1 = node1->next)
    {
      varpool_node *vnode1 = dyn_cast <varpool_node *> (node1);
      if (vnode1 && vnode1->definition)
	return vnode1;
    }
  return NULL;
}
/* Walk all variables with definitions in current unit.  */
#define FOR_EACH_DEFINED_VARIABLE(node) \
   for ((node) = varpool_first_defined_variable (); (node); \
        (node) = varpool_next_defined_variable (node))

/* Return first function with body defined.  */
static inline cgraph_node *
cgraph_first_defined_function (void)
{
  symtab_node *node;
  for (node = symtab_nodes; node; node = node->next)
    {
      cgraph_node *cn = dyn_cast <cgraph_node *> (node);
      if (cn && cn->definition)
	return cn;
    }
  return NULL;
}

/* Return next function with body defined after NODE.  */
static inline cgraph_node *
cgraph_next_defined_function (cgraph_node *node)
{
  symtab_node *node1 = node->next;
  for (; node1; node1 = node1->next)
    {
      cgraph_node *cn1 = dyn_cast <cgraph_node *> (node1);
      if (cn1 && cn1->definition)
	return cn1;
    }
  return NULL;
}

/* Walk all functions with body defined.  */
#define FOR_EACH_DEFINED_FUNCTION(node) \
   for ((node) = cgraph_first_defined_function (); (node); \
        (node) = cgraph_next_defined_function ((node)))

/* Return first function.  */
static inline cgraph_node *
cgraph_first_function (void)
{
  symtab_node *node;
  for (node = symtab_nodes; node; node = node->next)
    if (cgraph_node *cn = dyn_cast <cgraph_node *> (node))
      return cn;
  return NULL;
}

/* Return next function.  */
static inline cgraph_node *
cgraph_next_function (cgraph_node *node)
{
  symtab_node *node1 = node->next;
  for (; node1; node1 = node1->next)
    if (cgraph_node *cn1 = dyn_cast <cgraph_node *> (node1))
      return cn1;
  return NULL;
}
/* Walk all functions.  */
#define FOR_EACH_FUNCTION(node) \
   for ((node) = cgraph_first_function (); (node); \
        (node) = cgraph_next_function ((node)))

/* Return true when callgraph node is a function with Gimple body defined
   in current unit.  Functions can also be define externally or they
   can be thunks with no Gimple representation.

   Note that at WPA stage, the function body may not be present in memory.  */

inline bool
cgraph_node::has_gimple_body_p (void)
{
  return definition && !thunk.thunk_p && !alias;
}

/* Return first function with body defined.  */
static inline cgraph_node *
cgraph_first_function_with_gimple_body (void)
{
  symtab_node *node;
  for (node = symtab_nodes; node; node = node->next)
    {
      cgraph_node *cn = dyn_cast <cgraph_node *> (node);
      if (cn && cn->has_gimple_body_p ())
	return cn;
    }
  return NULL;
}

/* Return next reachable static variable with initializer after NODE.  */
static inline cgraph_node *
cgraph_next_function_with_gimple_body (cgraph_node *node)
{
  symtab_node *node1 = node->next;
  for (; node1; node1 = node1->next)
    {
      cgraph_node *cn1 = dyn_cast <cgraph_node *> (node1);
      if (cn1 && cn1->has_gimple_body_p ())
	return cn1;
    }
  return NULL;
}

/* Walk all functions with body defined.  */
#define FOR_EACH_FUNCTION_WITH_GIMPLE_BODY(node) \
   for ((node) = cgraph_first_function_with_gimple_body (); (node); \
        (node) = cgraph_next_function_with_gimple_body (node))

/* Create a new static variable of type TYPE.  */
tree add_new_static_var (tree type);

/* Uniquize all constants that appear in memory.
   Each constant in memory thus far output is recorded
   in `const_desc_table'.  */

struct GTY(()) constant_descriptor_tree {
  /* A MEM for the constant.  */
  rtx rtl;

  /* The value of the constant.  */
  tree value;

  /* Hash of value.  Computing the hash from value each time
     hashfn is called can't work properly, as that means recursive
     use of the hash table during hash table expansion.  */
  hashval_t hash;
};

/* Return true when function is only called directly or it has alias.
   i.e. it is not externally visible, address was not taken and
   it is not used in any other non-standard way.  */

inline bool
cgraph_node::only_called_directly_or_aliased_p (void)
{
  gcc_assert (!global.inlined_to);
  return (!force_output && !address_taken
	  && !used_from_other_partition
	  && !DECL_VIRTUAL_P (decl)
	  && !DECL_STATIC_CONSTRUCTOR (decl)
	  && !DECL_STATIC_DESTRUCTOR (decl)
	  && !externally_visible);
}

/* Return true when variable can be removed from variable pool
   if all direct calls are eliminated.  */

inline bool
varpool_node::can_remove_if_no_refs_p (void)
{
  if (DECL_EXTERNAL (decl))
    return true;
  return (!force_output && !used_from_other_partition
	  && ((DECL_COMDAT (decl)
	       && !forced_by_abi
	       && !used_from_object_file_p ())
	      || !externally_visible
	      || DECL_HAS_VALUE_EXPR_P (decl)));
}

/* Return true when all references to variable must be visible in ipa_ref_list.
   i.e. if the variable is not externally visible or not used in some magic
   way (asm statement or such).
   The magic uses are all summarized in force_output flag.  */

inline bool
varpool_node::all_refs_explicit_p ()
{
  return (definition
	  && !externally_visible
	  && !used_from_other_partition
	  && !force_output);
}

/* Constant pool accessor function.  */
htab_t constant_pool_htab (void);

/* Return node that alias is aliasing.  */

inline cgraph_node *
cgraph_node::get_alias_target (void)
{
  return dyn_cast <cgraph_node *> (symtab_node::get_alias_target ());
}

/* Return node that alias is aliasing.  */

inline varpool_node *
varpool_node::get_alias_target (void)
{
  return dyn_cast <varpool_node *> (symtab_node::get_alias_target ());
}

/* Given function symbol, walk the alias chain to return the function node
   is alias of. Do not walk through thunks.
   When AVAILABILITY is non-NULL, get minimal availability in the chain.  */

inline cgraph_node *
cgraph_node::ultimate_alias_target (enum availability *availability)
{
  cgraph_node *n = dyn_cast <cgraph_node *> (symtab_node::ultimate_alias_target
    (availability));
  if (!n && availability)
    *availability = AVAIL_NOT_AVAILABLE;
  return n;
}

/* For given variable pool node, walk the alias chain to return the function
   the variable is alias of. Do not walk through thunks.
   When AVAILABILITY is non-NULL, get minimal availability in the chain.  */

inline varpool_node *
varpool_node::ultimate_alias_target (availability *availability)
{
  varpool_node *n = dyn_cast <varpool_node *>
    (symtab_node::ultimate_alias_target (availability));

  if (!n && availability)
    *availability = AVAIL_NOT_AVAILABLE;
  return n;
}

/* Return true when the edge E represents a direct recursion.  */
static inline bool
cgraph_edge_recursive_p (struct cgraph_edge *e)
{
  cgraph_node *callee = e->callee->ultimate_alias_target ();
  if (e->caller->global.inlined_to)
    return e->caller->global.inlined_to->decl == callee->decl;
  else
    return e->caller->decl == callee->decl;
}

/* Return true if the TM_CLONE bit is set for a given FNDECL.  */
static inline bool
decl_is_tm_clone (const_tree fndecl)
{
  cgraph_node *n = cgraph_node::get (fndecl);
  if (n)
    return n->tm_clone;
  return false;
}

/* Likewise indicate that a node is needed, i.e. reachable via some
   external means.  */

inline void
cgraph_node::mark_force_output (void)
{
  force_output = 1;
  gcc_checking_assert (!global.inlined_to);
}

#endif  /* GCC_CGRAPH_H  */
