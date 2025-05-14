/* Callgraph handling code.
   Copyright (C) 2003-2025 Free Software Foundation, Inc.
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

#include "profile-count.h"
#include "ipa-ref.h"
#include "plugin-api.h"
#include "ipa-param-manipulation.h"

extern void debuginfo_early_init (void);
extern void debuginfo_init (void);
extern void debuginfo_fini (void);
extern void debuginfo_start (void);
extern void debuginfo_stop (void);
extern void debuginfo_early_start (void);
extern void debuginfo_early_stop (void);

class ipa_opt_pass_d;
typedef ipa_opt_pass_d *ipa_opt_pass;

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

struct GTY((for_user)) section_hash_entry
{
  int ref_count;
  char *name;  /* As long as this datastructure stays in GGC, we cannot put
		  string at the tail of structure of GGC dies in horrible
		  way  */
};

struct section_name_hasher : ggc_ptr_hash<section_hash_entry>
{
  typedef const char *compare_type;

  static hashval_t hash (section_hash_entry *);
  static bool equal (section_hash_entry *, const char *);
};

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
     explicitly visible within current unit (i.e. it's address is never taken
     and it is not exported to other units). Currently used only for
     functions.  */
  AVAIL_LOCAL
};

/* Classification of symbols WRT partitioning.  */
enum symbol_partitioning_class
{
   /* External declarations are ignored by partitioning algorithms and they are
      added into the boundary later via compute_ltrans_boundary.  */
   SYMBOL_EXTERNAL,
   /* Partitioned symbols are put into one of partitions.  */
   SYMBOL_PARTITION,
   /* Duplicated symbols (such as comdat or constant pool references) are
      copied into every node needing them via add_symbol_to_partition.  */
   SYMBOL_DUPLICATE
};

/* Base of all entries in the symbol table.
   The symtab_node is inherited by cgraph and varpol nodes.  */
struct GTY((desc ("%h.type"), tag ("SYMTAB_SYMBOL"),
	    chain_next ("%h.next"), chain_prev ("%h.previous")))
  symtab_node
{
public:
  friend class symbol_table;

  /* Constructor.  */
  explicit symtab_node (symtab_type t)
    : type (t), resolution (LDPR_UNKNOWN), definition (false), alias (false),
      transparent_alias (false), weakref (false), cpp_implicit_alias (false),
      symver (false), analyzed (false), writeonly (false),
      refuse_visibility_changes (false), externally_visible (false),
      no_reorder (false), force_output (false), forced_by_abi (false),
      unique_name (false), implicit_section (false), body_removed (false),
      semantic_interposition (flag_semantic_interposition),
      used_from_other_partition (false), in_other_partition (false),
      address_taken (false), in_init_priority_hash (false),
      need_lto_streaming (false), offloadable (false), ifunc_resolver (false),
      order (-1), next_sharing_asm_name (NULL),
      previous_sharing_asm_name (NULL), same_comdat_group (NULL), ref_list (),
      alias_target (NULL), lto_file_data (NULL), aux (NULL),
      x_comdat_group (NULL_TREE), x_section (NULL), m_uid (-1)
  {}

  /* Return name.  */
  const char *name () const;

  /* Return dump name.  */
  const char *dump_name () const;

  /* Return asm name.  */
  const char *asm_name () const;

  /* Return dump name with assembler name.  */
  const char *dump_asm_name () const;

  /* Return visibility name.  */
  const char *get_visibility_string () const;

  /* Return type_name name.  */
  const char *get_symtab_type_string () const;

  /* Add node into symbol table.  This function is not used directly, but via
     cgraph/varpool node creation routines.  */
  void register_symbol (void);

  /* Remove symbol from symbol table.  */
  void remove (void);

  /* Undo any definition or use of the symbol.  */
  void reset (bool preserve_comdat_group = false);

  /* Dump symtab node to F.  */
  void dump (FILE *f);

  /* Dump symtab callgraph in graphviz format.  */
  void dump_graphviz (FILE *f);

  /* Dump symtab node to stderr.  */
  void DEBUG_FUNCTION debug (void);

  /* Verify consistency of node.  */
  void DEBUG_FUNCTION verify (void);

  /* Return ipa reference from this symtab_node to
     REFERRED_NODE or REFERRED_VARPOOL_NODE. USE_TYPE specify type
     of the use and STMT the statement (if it exists).  */
  ipa_ref *create_reference (symtab_node *referred_node,
			     enum ipa_ref_use use_type);

  /* Return ipa reference from this symtab_node to
     REFERRED_NODE or REFERRED_VARPOOL_NODE. USE_TYPE specify type
     of the use and STMT the statement (if it exists).  */
  ipa_ref *create_reference (symtab_node *referred_node,
			     enum ipa_ref_use use_type, gimple *stmt);

  /* If VAL is a reference to a function or a variable, add a reference from
     this symtab_node to the corresponding symbol table node.  Return the new
     reference or NULL if none was created.  */
  ipa_ref *maybe_create_reference (tree val, gimple *stmt);

  /* Clone all references from symtab NODE to this symtab_node.  */
  void clone_references (symtab_node *node);

  /* Remove all stmt references in non-speculative references.
     Those are not maintained during inlining & clonning.
     The exception are speculative references that are updated along
     with callgraph edges associated with them.  */
  void clone_referring (symtab_node *node);

  /* Clone reference REF to this symtab_node and set its stmt to STMT.  */
  ipa_ref *clone_reference (ipa_ref *ref, gimple *stmt);

  /* Find the structure describing a reference to REFERRED_NODE of USE_TYPE and
     associated with statement STMT or LTO_STMT_UID.  */
  ipa_ref *find_reference (symtab_node *referred_node, gimple *stmt,
			   unsigned int lto_stmt_uid,
			   enum ipa_ref_use use_type);

  /* Remove all references that are associated with statement STMT.  */
  void remove_stmt_references (gimple *stmt);

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

  /* Get number of references for this node.  */
  inline unsigned num_references (void)
  {
    return ref_list.references.length ();
  }

  /* Iterates I-th reference in the list, REF is also set.  */
  ipa_ref *iterate_reference (unsigned i, ipa_ref *&ref);

  /* Iterates I-th referring item in the list, REF is also set.  */
  ipa_ref *iterate_referring (unsigned i, ipa_ref *&ref);

  /* Iterates I-th referring alias item in the list, REF is also set.  */
  ipa_ref *iterate_direct_aliases (unsigned i, ipa_ref *&ref);

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
  const char * get_section () const
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
     When AVAILABILITY is non-NULL, get minimal availability in the chain.
     When REF is non-NULL, assume that reference happens in symbol REF
     when determining the availability.  */
  symtab_node *ultimate_alias_target (enum availability *avail = NULL,
				      struct symtab_node *ref = NULL);

  /* Return next reachable static symbol with initializer after NODE.  */
  inline symtab_node *next_defined_symbol (void);

  /* Add reference recording that symtab node is alias of TARGET.
     If TRANSPARENT is true make the alias to be transparent alias.
     The function can fail in the case of aliasing cycles; in this case
     it returns false.  */
  bool resolve_alias (symtab_node *target, bool transparent = false);

  /* C++ FE sometimes change linkage flags after producing same
     body aliases.  */
  void fixup_same_cpp_alias_visibility (symtab_node *target);

  /* Call callback on symtab node and aliases associated to this node.
     When INCLUDE_OVERWRITABLE is false, overwritable aliases and thunks are
     skipped.  */
  bool call_for_symbol_and_aliases (bool (*callback) (symtab_node *, void *),
				    void *data,
				    bool include_overwrite);

  /* If node cannot be interposable by static or dynamic linker to point to
     different definition, return this symbol. Otherwise look for alias with
     such property and if none exists, introduce new one.  */
  symtab_node *noninterposable_alias (void);

  /* Return node that alias is aliasing.  */
  inline symtab_node *get_alias_target (void);

  /* Return DECL that alias is aliasing.  */
  inline tree get_alias_target_tree ();

  /* Set section for symbol and its aliases.  */
  void set_section (const char *section);

  /* Like set_section, but copying the section name from another node.  */
  void set_section (const symtab_node &other);

  /* Set section, do not recurse into aliases.
     When one wants to change section of symbol and its aliases,
     use set_section.  */
  void set_section_for_node (const char *section);

  /* Like set_section_for_node, but copying the section name from another
     node.  */
  void set_section_for_node (const symtab_node &other);

  /* Set initialization priority to PRIORITY.  */
  void set_init_priority (priority_type priority);

  /* Return the initialization priority.  */
  priority_type get_init_priority ();

  /* Return availability of NODE when referenced from REF.  */
  enum availability get_availability (symtab_node *ref = NULL);

  /* During LTO stream-in this predicate can be used to check whether node
     in question prevails in the linking to save some memory usage.  */
  bool prevailing_p (void);

  /* Return true if NODE binds to current definition in final executable
     when referenced from REF.  If REF is NULL return conservative value
     for any reference.  */
  bool binds_to_current_def_p (symtab_node *ref = NULL);

  /* Make DECL local.  */
  void make_decl_local (void);

  /* Copy visibility from N.  */
  void copy_visibility_from (symtab_node *n);

  /* Return desired alignment of the definition.  This is NOT alignment useful
     to access THIS, because THIS may be interposable and DECL_ALIGN should
     be used instead.  It however must be guaranteed when output definition
     of THIS.  */
  unsigned int definition_alignment ();

  /* Return true if alignment can be increased.  */
  bool can_increase_alignment_p ();

  /* Increase alignment of symbol to ALIGN.  */
  void increase_alignment (unsigned int align);

  /* Return true if list contains an alias.  */
  bool has_aliases_p (void);

  /* Return true when the symbol is real symbol, i.e. it is not inline clone
     or abstract function kept for debug info purposes only.  */
  bool real_symbol_p (void);

  /* Return true when the symbol needs to be output to the LTO symbol table.  */
  bool output_to_lto_symbol_table_p (void);

  /* Determine if symbol declaration is needed.  That is, visible to something
     either outside this translation unit, something magic in the system
     configury. This function is used just during symbol creation.  */
  bool needed_p (void);

  /* Return true if this symbol is a function from the C frontend specified
     directly in RTL form (with "__RTL").  */
  bool native_rtl_p () const;

  /* Return true when there are references to the node.  */
  bool referred_to_p (bool include_self = true);

  /* Return true if symbol can be discarded by linker from the binary.
     Assume that symbol is used (so there is no need to take into account
     garbage collecting linkers)

     This can happen for comdats, commons and weaks when they are prevailed
     by other definition at static linking time.  */
  inline bool
  can_be_discarded_p (void)
  {
    return ((DECL_EXTERNAL (decl)
	     && !in_other_partition)
	    || ((get_comdat_group ()
		 || DECL_COMMON (decl)
		 || (DECL_SECTION_NAME (decl) && DECL_WEAK (decl)))
		&& ((resolution != LDPR_PREVAILING_DEF
		     && resolution != LDPR_PREVAILING_DEF_IRONLY_EXP)
		    || flag_incremental_link)
		&& resolution != LDPR_PREVAILING_DEF_IRONLY));
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

  /* Return true if symbol is known to be nonzero, assume that
     flag_delete_null_pointer_checks is equal to delete_null_pointer_checks.  */
  bool nonzero_address (bool delete_null_pointer_checks);

  /* Return true if symbol is known to be nonzero.  */
  bool nonzero_address ();

  /* Return 0 if symbol is known to have different address than S2,
     Return 1 if symbol is known to have same address as S2,
     return 2 otherwise.

     If MEMORY_ACCESSED is true, assume that both memory pointer to THIS
     and S2 is going to be accessed.  This eliminates the situations when
     either THIS or S2 is NULL and is useful for comparing bases when deciding
     about memory aliasing.  */
  int equal_address_to (symtab_node *s2, bool memory_accessed = false);

  /* Return true if symbol's address may possibly be compared to other
     symbol's address.  */
  bool address_matters_p ();

  /* Return true if NODE's address can be compared.  This use properties
     of NODE only and does not look if the address is actually taken in
     interesting way.  For that use ADDRESS_MATTERS_P instead.  */
  bool address_can_be_compared_p (void);

  /* Return symbol table node associated with DECL, if any,
     and NULL otherwise.  */
  static inline symtab_node *get (const_tree decl)
  {
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
    return decl->decl_with_vis.symtab_node;
  }

  /* Try to find a symtab node for declaration DECL and if it does not
     exist or if it corresponds to an inline clone, create a new one.  */
  static inline symtab_node * get_create (tree node);

  /* Return the cgraph node that has ASMNAME for its DECL_ASSEMBLER_NAME.
     Return NULL if there's no such node.  */
  static symtab_node *get_for_asmname (const_tree asmname);

  /* Check symbol table for callees of IFUNC resolvers.  */
  static void check_ifunc_callee_symtab_nodes (void);

  /* Verify symbol table for internal consistency.  */
  static DEBUG_FUNCTION void verify_symtab_nodes (void);

  /* Perform internal consistency checks, if they are enabled.  */
  static inline void checking_verify_symtab_nodes (void);

  /* Get unique identifier of the node.  */
  inline int get_uid ()
  {
    return m_uid;
  }

  /* Type of the symbol.  */
  ENUM_BITFIELD (symtab_type) type : 8;

  /* The symbols resolution.  */
  ENUM_BITFIELD (ld_plugin_symbol_resolution) resolution : 8;

  /*** Flags representing the symbol type.  ***/

  /* True when symbol corresponds to a definition in current unit.
     set via finalize_function or finalize_decl  */
  unsigned definition : 1;
  /* True when symbol is an alias.
     Set by assemble_alias.  */
  unsigned alias : 1;
  /* When true the alias is translated into its target symbol either by GCC
     or assembler (it also may just be a duplicate declaration of the same
     linker name).

     Currently transparent aliases come in three different flavors
       - aliases having the same assembler name as their target (aka duplicated
	 declarations). In this case the assembler names compare via
	 assembler_names_equal_p and weakref is false
       - aliases that are renamed at a time being output to final file
	 by varasm.cc. For those DECL_ASSEMBLER_NAME have
	 IDENTIFIER_TRANSPARENT_ALIAS set and thus also their assembler
	 name must be unique.
	 Weakrefs belong to this category when we target assembler without
	 .weakref directive.
       - weakrefs that are renamed by assembler via .weakref directive.
	 In this case the alias may or may not be definition (depending if
	 target declaration was seen by the compiler), weakref is set.
	 Unless we are before renaming statics, assembler names are different.

     Given that we now support duplicate declarations, the second option is
     redundant and will be removed.  */
  unsigned transparent_alias : 1;
  /* True when alias is a weakref.  */
  unsigned weakref : 1;
  /* C++ frontend produce same body aliases and extra name aliases for
     virtual functions and vtables that are obviously equivalent.
     Those aliases are bit special, especially because C++ frontend
     visibility code is so ugly it cannot get them right at first time
     and their visibility needs to be copied from their "masters" at
     the end of parsing.  */
  unsigned cpp_implicit_alias : 1;
  /* The alias is a symbol version.  */
  unsigned symver : 1;
  /* Set once the definition was analyzed.  The list of references and
     other properties are built during analysis.  */
  unsigned analyzed : 1;
  /* Set for write-only variables.  */
  unsigned writeonly : 1;
  /* Visibility of symbol was used for further optimization; do not
     permit further changes.  */
  unsigned refuse_visibility_changes : 1;

  /*** Visibility and linkage flags.  ***/

  /* Set when function is visible by other units.  */
  unsigned externally_visible : 1;
  /* Don't reorder to other symbols having this set.  */
  unsigned no_reorder : 1;
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
  /* True when symbol should comply to -fsemantic-interposition flag.  */
  unsigned semantic_interposition : 1;

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

  /* Set when symbol needs to be streamed into LTO bytecode for LTO, or in case
     of offloading, for separate compilation for a different target.  */
  unsigned need_lto_streaming : 1;

  /* Set when symbol can be streamed into bytecode for offloading.  */
  unsigned offloadable : 1;

  /* Set when symbol is an IFUNC resolver.  */
  unsigned ifunc_resolver : 1;


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
  ipa_ref_list GTY((skip)) ref_list;

  /* Alias target. May be either DECL pointer or ASSEMBLER_NAME pointer
     depending to what was known to frontend on the creation time.
     Once alias is resolved, this pointer become NULL.  */
  tree alias_target;

  /* File stream where this node is being written to.  */
  struct lto_file_decl_data * lto_file_data;

  void *GTY ((skip)) aux;

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
  void unregister (struct clone_info *);

  /* Return the initialization and finalization priority information for
     DECL.  If there is no previous priority information, a freshly
     allocated structure is returned.  */
  struct symbol_priority_map *priority_info (void);

  /* Worker for call_for_symbol_and_aliases_1.  */
  bool call_for_symbol_and_aliases_1 (bool (*callback) (symtab_node *, void *),
				      void *data,
				      bool include_overwrite);
private:
  /* Unique id of the node.  */
  int m_uid;

  /* Workers for set_section.  */
  static bool set_section_from_string (symtab_node *n, void *s);
  static bool set_section_from_node (symtab_node *n, void *o);

  /* Worker for symtab_resolve_alias.  */
  static bool set_implicit_section (symtab_node *n, void *);

  /* Worker searching noninterposable alias.  */
  static bool noninterposable_alias (symtab_node *node, void *data);

  /* Worker for ultimate_alias_target.  */
  symtab_node *ultimate_alias_target_1 (enum availability *avail = NULL,
					symtab_node *ref = NULL);

  /* Get dump name with normal or assembly name.  */
  const char *get_dump_name (bool asm_name_p) const;
};

inline void
symtab_node::checking_verify_symtab_nodes (void)
{
  if (flag_checking)
    symtab_node::verify_symtab_nodes ();
}

/* Walk all aliases for NODE.  */
#define FOR_EACH_ALIAS(NODE, ALIAS)				\
  for (unsigned ALIAS##_iter_ = 0;				\
       (NODE)->iterate_direct_aliases (ALIAS##_iter_, ALIAS);	\
       ALIAS##_iter_++)

/* This is the information that is put into the cgraph local structure
   to recover a function.  */
struct lto_file_decl_data;

extern const char * const cgraph_availability_names[];
extern const char * const ld_plugin_symbol_resolution_names[];
extern const char * const tls_model_names[];

/* Represent which DECL tree (or reference to such tree)
   will be replaced by another tree while versioning.  */
struct GTY(()) ipa_replace_map
{
  /* The new (replacing) tree.  */
  tree new_tree;
  /* Parameter number to replace, when old_tree is NULL.  */
  int parm_num;
  /* Set if the newly added reference should not be an address one, but a load
     one from the operand of the ADDR_EXPR in NEW_TREE.  This is for cases when
     the corresponding parameter p is used only as *p.  */
  unsigned force_load_ref : 1;
};

enum cgraph_simd_clone_arg_type
{
  SIMD_CLONE_ARG_TYPE_VECTOR,
  SIMD_CLONE_ARG_TYPE_UNIFORM,
  /* These are only for integer/pointer arguments passed by value.  */
  SIMD_CLONE_ARG_TYPE_LINEAR_CONSTANT_STEP,
  SIMD_CLONE_ARG_TYPE_LINEAR_VARIABLE_STEP,
  /* These 6 are only for reference type arguments or arguments passed
     by reference.  */
  SIMD_CLONE_ARG_TYPE_LINEAR_REF_CONSTANT_STEP,
  SIMD_CLONE_ARG_TYPE_LINEAR_REF_VARIABLE_STEP,
  SIMD_CLONE_ARG_TYPE_LINEAR_UVAL_CONSTANT_STEP,
  SIMD_CLONE_ARG_TYPE_LINEAR_UVAL_VARIABLE_STEP,
  SIMD_CLONE_ARG_TYPE_LINEAR_VAL_CONSTANT_STEP,
  SIMD_CLONE_ARG_TYPE_LINEAR_VAL_VARIABLE_STEP,
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

  /* Variable alignment if available, otherwise 0.  */
  unsigned int alignment;

  /* For arg_type SIMD_CLONE_ARG_TYPE_LINEAR_*CONSTANT_STEP this is
     the constant linear step, if arg_type is
     SIMD_CLONE_ARG_TYPE_LINEAR_*VARIABLE_STEP, this is index of
     the uniform argument holding the step, otherwise 0.  */
  HOST_WIDE_INT linear_step;
};

/* Specific data for a SIMD function clone.  */

struct GTY(()) cgraph_simd_clone {
  /* Number of words in the SIMD lane associated with this clone.  */
  poly_uint64 simdlen;

  /* Number of annotated function arguments in `args'.  This is
     usually the number of named arguments in FNDECL.  */
  unsigned int nargs;

  /* Max hardware vector size in bits for integral vectors.  */
  poly_uint64 vecsize_int;

  /* Max hardware vector size in bits for floating point vectors.  */
  poly_uint64 vecsize_float;

  /* Machine mode of the mask argument(s), if they are to be passed
     as bitmasks in integer argument(s).  VOIDmode if masks are passed
     as vectors of characteristic type.  */
  machine_mode mask_mode;

  /* The mangling character for a given vector size.  This is used
     to determine the ISA mangling bit as specified in the Intel
     Vector ABI.  */
  unsigned char vecsize_mangle;

  /* True if this is the masked, in-branch version of the clone,
     otherwise false.  */
  unsigned int inbranch : 1;

  /* Doubly linked list of SIMD clones.  */
  cgraph_node *prev_clone, *next_clone;

  /* Original cgraph node the SIMD clones were created for.  */
  cgraph_node *origin;

  /* Annotated function arguments for the original function.  */
  cgraph_simd_clone_arg GTY((length ("%h.nargs"))) args[1];
};

/* Function Multiversioning info.  */
struct GTY((for_user)) cgraph_function_version_info {
  /* The cgraph_node for which the function version info is stored.  */
  cgraph_node *this_node;
  /* Chains all the semantically identical function versions.  The
     first function in this chain is the version_info node of the
     default function.  */
  cgraph_function_version_info *prev;
  /* If this version node corresponds to a dispatcher for function
     versions, this points to the version info node of the default
     function, the first node in the chain.  */
  cgraph_function_version_info *next;
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

struct cgraph_edge_hasher : ggc_ptr_hash<cgraph_edge>
{
  typedef gimple *compare_type;

  static hashval_t hash (cgraph_edge *);
  static hashval_t hash (gimple *);
  static bool equal (cgraph_edge *, gimple *);
};

/* The cgraph data structure.
   Each function decl has assigned cgraph_node listing callees and callers.  */

struct GTY((tag ("SYMTAB_FUNCTION"))) cgraph_node : public symtab_node
{
  friend class symbol_table;

  /* Constructor.  */
  explicit cgraph_node ()
    : symtab_node (SYMTAB_FUNCTION), callees (NULL), callers (NULL),
      indirect_calls (NULL),
      next_sibling_clone (NULL), prev_sibling_clone (NULL), clones (NULL),
      clone_of (NULL), call_site_hash (NULL), former_clone_of (NULL),
      simdclone (NULL), simd_clones (NULL), ipa_transforms_to_apply (vNULL),
      inlined_to (NULL), rtl (NULL),
      count (profile_count::uninitialized ()),
      count_materialization_scale (REG_BR_PROB_BASE), profile_id (0),
      unit_id (0), tp_first_run (0), thunk (false),
      used_as_abstract_origin (false),
      lowered (false), process (false), frequency (NODE_FREQUENCY_NORMAL),
      only_called_at_startup (false), only_called_at_exit (false),
      tm_clone (false), dispatcher_function (false), calls_comdat_local (false),
      icf_merged (false), nonfreeing_fn (false), merged_comdat (false),
      merged_extern_inline (false), parallelized_function (false),
      split_part (false), indirect_call_target (false), local (false),
      versionable (false), can_change_signature (false),
      redefined_extern_inline (false), tm_may_enter_irr (false),
      ipcp_clone (false), gc_candidate (false),
      called_by_ifunc_resolver (false), has_omp_variant_constructs (false),
      m_summary_id (-1)
  {}

  /* Remove the node from cgraph and all inline clones inlined into it.
     Skip however removal of FORBIDDEN_NODE and return true if it needs to be
     removed.  This allows to call the function from outer loop walking clone
     tree.  */
  bool remove_symbol_and_inline_clones (cgraph_node *forbidden_node = NULL);

  /* Record all references from cgraph_node that are taken
     in statement STMT.  */
  void record_stmt_references (gimple *stmt);

  /* Like cgraph_set_call_stmt but walk the clone tree and update all
     clones sharing the same function body.
     When WHOLE_SPECULATIVE_EDGES is true, all three components of
     speculative edge gets updated.  Otherwise we update only direct
     call.  */
  void set_call_stmt_including_clones (gimple *old_stmt, gcall *new_stmt,
				       bool update_speculative = true);

  /* Walk the alias chain to return the function cgraph_node is alias of.
     Walk through thunk, too.
     When AVAILABILITY is non-NULL, get minimal availability in the chain.
     When REF is non-NULL, assume that reference happens in symbol REF
     when determining the availability.  */
  cgraph_node *function_symbol (enum availability *avail = NULL,
				struct symtab_node *ref = NULL);

  /* Walk the alias chain to return the function cgraph_node is alias of.
     Walk through non virtual thunks, too.  Thus we return either a function
     or a virtual thunk node.
     When AVAILABILITY is non-NULL, get minimal availability in the chain.
     When REF is non-NULL, assume that reference happens in symbol REF
     when determining the availability.  */
  cgraph_node *function_or_virtual_thunk_symbol
				(enum availability *avail = NULL,
				 struct symtab_node *ref = NULL);

  /* Create node representing clone of N executed COUNT times.  Decrease
     the execution counts from original node too.
     The new clone will have decl set to DECL that may or may not be the same
     as decl of N.

     When UPDATE_ORIGINAL is true, the counts are subtracted from the original
     function's profile to reflect the fact that part of execution is handled
     by node.
     When CALL_DUPLICATION_HOOK is true, the ipa passes are acknowledged about
     the new clone. Otherwise the caller is responsible for doing so later.

     If the new node is being inlined into another one, NEW_INLINED_TO should be
     the outline function the new one is (even indirectly) inlined to.
     All hooks will see this in node's inlined_to, when invoked.
     Should be NULL if the node is not inlined.

     SUFFIX is string that is appended to the original name, it should only be
     NULL if NEW_INLINED_TO is not NULL or if the clone being created is
     temporary and a record about it should not be added into the ipa-clones
     dump file.  */
  cgraph_node *create_clone (tree decl, profile_count count,
			     bool update_original,
			     vec<cgraph_edge *> redirect_callers,
			     bool call_duplication_hook,
			     cgraph_node *new_inlined_to,
			     ipa_param_adjustments *param_adjustments,
			     const char *suffix);

  /* Create callgraph node clone with new declaration.  The actual body will be
     copied later at compilation stage.  The name of the new clone will be
     constructed from the name of the original node, SUFFIX and NUM_SUFFIX.  */
  cgraph_node *create_virtual_clone (const vec<cgraph_edge *> &redirect_callers,
				     vec<ipa_replace_map *, va_gc> *tree_map,
				     ipa_param_adjustments *param_adjustments,
				     const char * suffix, unsigned num_suffix);

  /* Remove the node from the tree of virtual and inline clones and make it a
     standalone node - not a clone any more.  */
  void remove_from_clone_tree ();

  /* cgraph node being removed from symbol table; see if its entry can be
   replaced by other inline clone.  */
  cgraph_node *find_replacement (struct clone_info *);

  /* Create a new cgraph node which is the new version of
     callgraph node.  REDIRECT_CALLERS holds the callers
     edges which should be redirected to point to
     NEW_VERSION.  ALL the callees edges of the node
     are cloned to the new version node.  Return the new
     version node.

     If non-NULL BLOCK_TO_COPY determine what basic blocks
     was copied to prevent duplications of calls that are dead
     in the clone.

     SUFFIX is string that is appended to the original name.  */

  cgraph_node *create_version_clone (tree new_decl,
				    vec<cgraph_edge *> redirect_callers,
				    bitmap bbs_to_copy,
				    const char *suffix = NULL);

  /* Perform function versioning.
     Function versioning includes copying of the tree and
     a callgraph update (creating a new cgraph node and updating
     its callees and callers).

     REDIRECT_CALLERS varray includes the edges to be redirected
     to the new version.

     TREE_MAP is a mapping of tree nodes we want to replace with
     new ones (according to results of prior analysis).

     If non-NULL PARAM_ADJUSTMENTS determine how function formal parameters
     should be modified in the new version and if it should return void.
     If non-NULL BLOCK_TO_COPY determine what basic blocks to copy.
     If non_NULL NEW_ENTRY determine new entry BB of the clone.
     SUFFIX is a string that will be used to create a new name for the new
     function.

     If TARGET_ATTRIBUTES is non-null, when creating a new declaration,
     add the attributes to DECL_ATTRIBUTES.  And call valid_attribute_p
     that will promote value of the attribute DECL_FUNCTION_SPECIFIC_TARGET
     of the declaration.

     If VERSION_DECL is set true, use clone_function_name_numbered for the
     function clone.  Otherwise, use clone_function_name.

     Return the new version's cgraph node.  */
  cgraph_node *create_version_clone_with_body
    (vec<cgraph_edge *> redirect_callers,
     vec<ipa_replace_map *, va_gc> *tree_map,
     ipa_param_adjustments *param_adjustments,
     bitmap bbs_to_copy, basic_block new_entry_block, const char *suffix,
     tree target_attributes = NULL_TREE, bool version_decl = true);

  /* Insert a new cgraph_function_version_info node into cgraph_fnver_htab
     corresponding to cgraph_node.  */
  cgraph_function_version_info *insert_new_function_version (void);

  /* Get the cgraph_function_version_info node corresponding to node.  */
  cgraph_function_version_info *function_version (void);

  /* Discover all functions and variables that are trivially needed, analyze
     them as well as all functions and variables referred by them  */
  void analyze (void);

  /* Add thunk alias into callgraph.  The alias declaration is ALIAS and it
     aliases DECL with an adjustments made into the first parameter.
     See comments in struct symtab-thunks.h for detail on the parameters.  */
  cgraph_node * create_thunk (tree alias, tree, bool this_adjusting,
			      HOST_WIDE_INT fixed_offset,
			      HOST_WIDE_INT virtual_value,
			      HOST_WIDE_INT indirect_offset,
			      tree virtual_offset,
			      tree real_alias);


  /* Return node that alias is aliasing.  */
  inline cgraph_node *get_alias_target (void);

  /* Given function symbol, walk the alias chain to return the function node
     is alias of. Do not walk through thunks.
     When AVAILABILITY is non-NULL, get minimal availability in the chain.
     When REF is non-NULL, assume that reference happens in symbol REF
     when determining the availability.  */

  cgraph_node *ultimate_alias_target (availability *availability = NULL,
				      symtab_node *ref = NULL);

  /*  Call expand_thunk on all callers that are thunks and analyze those
      nodes that were expanded.  */
  void expand_all_artificial_thunks ();

  /* Assemble thunks and aliases associated to node.  */
  void assemble_thunks_and_aliases (void);

  /* Expand function specified by node.  */
  void expand (void);

  /* Creates a wrapper from cgraph_node to TARGET node. Thunk is used for this
     kind of wrapper method.  */
  void create_wrapper (cgraph_node *target);

  /* Verify cgraph nodes of the cgraph node.  */
  void DEBUG_FUNCTION verify_node (void);

  /* Remove function from symbol table.  */
  void remove (void);

  /* Dump call graph node to file F.  */
  void dump (FILE *f);

  /* Dump call graph node to file F.  */
  void dump_graphviz (FILE *f);

  /* Dump call graph node to stderr.  */
  void DEBUG_FUNCTION debug (void);

  /* When doing LTO, read cgraph_node's body from disk if it is not already
     present.  */
  bool get_untransformed_body ();

  /* Prepare function body.  When doing LTO, read cgraph_node's body from disk
     if it is not already present.  When some IPA transformations are scheduled,
     apply them.  */
  bool get_body ();

  void materialize_clone (void);

  /* Release memory used to represent body of function.
     Use this only for functions that are released before being translated to
     target code (i.e. RTL).  Functions that are compiled to RTL and beyond
     are free'd in final.cc via free_after_compilation().  */
  void release_body (bool keep_arguments = false);

  /* Return the DECL_STRUCT_FUNCTION of the function.  */
  struct function *get_fun () const;

  /* Bring cgraph node local.  */
  void make_local (void);

  /* Likewise indicate that a node is having address taken.  */
  void mark_address_taken (void);

  /* Set finalization priority to PRIORITY.  */
  void set_fini_priority (priority_type priority);

  /* Return the finalization priority.  */
  priority_type get_fini_priority (void);

  /* Create edge from a given function to CALLEE in the cgraph.  */
  cgraph_edge *create_edge (cgraph_node *callee,
			    gcall *call_stmt, profile_count count,
			    bool cloning_p = false);

  /* Create an indirect edge with a yet-undetermined callee where the call
     statement destination is a formal parameter of the caller with index
     PARAM_INDEX. */
  cgraph_edge *create_indirect_edge (gcall *call_stmt, int ecf_flags,
				     profile_count count,
				     bool cloning_p = false);

  /* Like cgraph_create_edge walk the clone tree and update all clones sharing
   same function body.  If clones already have edge for OLD_STMT; only
   update the edge same way as cgraph_set_call_stmt_including_clones does.  */
  void create_edge_including_clones (cgraph_node *callee,
				     gimple *old_stmt, gcall *stmt,
				     profile_count count,
				     cgraph_inline_failed_t reason);

  /* Return the callgraph edge representing the GIMPLE_CALL statement
     CALL_STMT.  */
  cgraph_edge *get_edge (gimple *call_stmt);

  /* Collect all callers of cgraph_node and its aliases that are known to lead
     to NODE (i.e. are not overwritable) and that are not thunks.  */
  auto_vec<cgraph_edge *> collect_callers (void);

  /* Remove all callers from the node.  */
  void remove_callers (void);

  /* Remove all callees from the node.  */
  void remove_callees (void);

  /* Return function availability.  See cgraph.h for description of individual
     return values.  */
  enum availability get_availability (symtab_node *ref = NULL);

  /* Set TREE_NOTHROW on cgraph_node's decl and on aliases of the node
     if any to NOTHROW.  */
  bool set_nothrow_flag (bool nothrow);

  /* SET DECL_IS_MALLOC on cgraph_node's decl and on aliases of the node
     if any.  */
  bool set_malloc_flag (bool malloc_p);

  /* SET TREE_THIS_VOLATILE on cgraph_node's decl and on aliases of the node
     if any.  */
  bool set_noreturn_flag (bool noreturn_p);

  /* If SET_CONST is true, mark function, aliases and thunks to be ECF_CONST.
    If SET_CONST if false, clear the flag.

    When setting the flag be careful about possible interposition and
    do not set the flag for functions that can be interposed and set pure
    flag for functions that can bind to other definition.

    Return true if any change was done. */

  bool set_const_flag (bool set_const, bool looping);

  /* Set DECL_PURE_P on cgraph_node's decl and on aliases of the node
     if any to PURE.

     When setting the flag, be careful about possible interposition.
     Return true if any change was done. */

  bool set_pure_flag (bool pure, bool looping);

  /* Add attribute ATTR to cgraph_node's decl and on aliases of the node
     if any.  */
  bool add_detected_attribute (const char *attr);

  /* Call callback on function and aliases associated to the function.
     When INCLUDE_OVERWRITABLE is false, overwritable aliases and thunks are
     skipped. */

  bool call_for_symbol_and_aliases (bool (*callback) (cgraph_node *,
						      void *),
				    void *data, bool include_overwritable);

  /* Call callback on cgraph_node, thunks and aliases associated to NODE.
     When INCLUDE_OVERWRITABLE is false, overwritable aliases and thunks are
     skipped.  When EXCLUDE_VIRTUAL_THUNKS is true, virtual thunks are
     skipped.  */
  bool call_for_symbol_thunks_and_aliases (bool (*callback) (cgraph_node *node,
							     void *data),
					   void *data,
					   bool include_overwritable,
					   bool exclude_virtual_thunks = false);

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

  /* Return true when cgraph_node cannot return or throw and thus
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
     linkonce section.

     If WILL_INLINE is true, assume that function will be inlined into all the
     direct calls.  */
  bool will_be_removed_from_program_if_no_direct_calls_p
	 (bool will_inline = false);

  /* Return true when function can be removed from callgraph
     if all direct calls and references are eliminated.  The function does
     not take into account comdat groups.  */
  bool can_remove_if_no_direct_calls_and_refs_p (void);

  /* Return true when function cgraph_node and its aliases can be removed from
     callgraph if all direct calls are eliminated.
     If WILL_INLINE is true, assume that function will be inlined into all the
     direct calls.  */
  bool can_remove_if_no_direct_calls_p (bool will_inline = false);

  /* Return true when callgraph node is a function with Gimple body defined
     in current unit.  Functions can also be define externally or they
     can be thunks with no Gimple representation.

     Note that at WPA stage, the function body may not be present in memory.  */
  inline bool has_gimple_body_p (void);

  /* Return true if this node represents a former, i.e. an expanded, thunk.  */
  bool former_thunk_p (void);

  /* Check if function calls comdat local.  This is used to recompute
     calls_comdat_local flag after function transformations.  */
  bool check_calls_comdat_local_p ();

  /* Return true if function should be optimized for size.  */
  enum optimize_size_level optimize_for_size_p (void);

  /* Dump the callgraph to file F.  */
  static void dump_cgraph (FILE *f);

  /* Dump the call graph to stderr.  */
  static inline
  void debug_cgraph (void)
  {
    dump_cgraph (stderr);
  }

  /* Get summary id of the node.  */
  inline int get_summary_id ()
  {
    return m_summary_id;
  }

  /* Record that DECL1 and DECL2 are semantically identical function
     versions.  */
  static void record_function_versions (tree decl1, tree decl2);

  /* Remove the cgraph_function_version_info and cgraph_node for DECL.  This
     DECL is a duplicate declaration.  */
  static void delete_function_version_by_decl (tree decl);

  /* Add the function FNDECL to the call graph.
     Unlike finalize_function, this function is intended to be used
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

  /* DECL has been parsed.  Take it, queue it, compile it at the whim of the
     logic in effect.  If NO_COLLECT is true, then our caller cannot stand to
     have the garbage collector run at the moment.  We would need to either
     create a new GC context, or just not compile right now.  */
  static void finalize_function (tree, bool);

  /* Return cgraph node assigned to DECL.  Create new one when needed.  */
  static cgraph_node * create (tree decl);

  /* Try to find a call graph node for declaration DECL and if it does not
     exist or if it corresponds to an inline clone, create a new one.  */
  static cgraph_node * get_create (tree);

  /* Return local info for the compiled function.  */
  static cgraph_node *local_info_node (tree decl);

  /* Return RTL info for the compiled function.  */
  static struct cgraph_rtl_info *rtl_info (const_tree);

  /* Return the cgraph node that has ASMNAME for its DECL_ASSEMBLER_NAME.
     Return NULL if there's no such node.  */
  static cgraph_node *get_for_asmname (tree asmname);

  /* Attempt to mark ALIAS as an alias to DECL.  Return alias node if
     successful and NULL otherwise.
     Same body aliases are output whenever the body of DECL is output,
     and cgraph_node::get (ALIAS) transparently
     returns cgraph_node::get (DECL).  */
  static cgraph_node * create_same_body_alias (tree alias, tree decl);

  /* Verify whole cgraph structure.  */
  static void DEBUG_FUNCTION verify_cgraph_nodes (void);

  /* Verify cgraph, if consistency checking is enabled.  */
  static inline void checking_verify_cgraph_nodes (void);

  /* Worker to bring NODE local.  */
  static bool make_local (cgraph_node *node, void *);

  /* Mark ALIAS as an alias to DECL.  DECL_NODE is cgraph node representing
     the function body is associated
     with (not necessarily cgraph_node (DECL).  */
  static cgraph_node *create_alias (tree alias, tree target);

  /* Return true if NODE has thunk.  */
  static bool has_thunk_p (cgraph_node *node, void *);

  cgraph_edge *callees;
  cgraph_edge *callers;
  /* List of edges representing indirect calls with a yet undetermined
     callee.  */
  cgraph_edge *indirect_calls;
  cgraph_node *next_sibling_clone;
  cgraph_node *prev_sibling_clone;
  cgraph_node *clones;
  cgraph_node *clone_of;
  /* For functions with many calls sites it holds map from call expression
     to the edge to speed up cgraph_edge function.  */
  hash_table<cgraph_edge_hasher> *GTY(()) call_site_hash;
  /* Declaration node used to be clone of. */
  tree former_clone_of;

  /* If this is a SIMD clone, this points to the SIMD specific
     information for it.  */
  cgraph_simd_clone *simdclone;
  /* If this function has SIMD clones, this points to the first clone.  */
  cgraph_node *simd_clones;

  /* Interprocedural passes scheduled to have their transform functions
     applied next time we execute local pass on them.  We maintain it
     per-function in order to allow IPA passes to introduce new functions.  */
  vec<ipa_opt_pass, va_heap, vl_ptr> GTY((skip)) ipa_transforms_to_apply;

  /* For inline clones this points to the function they will be
     inlined into.  */
  cgraph_node *inlined_to;

  struct cgraph_rtl_info *rtl;

  /* Expected number of executions: calculated in profile.cc.  */
  profile_count count;
  /* How to scale counts at materialization time; used to merge
     LTO units with different number of profile runs.  */
  int count_materialization_scale;
  /* ID assigned by the profiling.  */
  unsigned int profile_id;
  /* ID of the translation unit.  */
  int unit_id;
  /* Time profiler: first run of function.  */
  int tp_first_run;

  /* True when symbol is a thunk.  */
  unsigned thunk : 1;
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
     compute_fn_summary and inline_call.  */
  unsigned calls_comdat_local : 1;
  /* True if node has been created by merge operation in IPA-ICF.  */
  unsigned icf_merged: 1;
  /* True if call to node can't result in a call to free, munmap or
     other operation that could make previously non-trapping memory
     accesses trapping.  */
  unsigned nonfreeing_fn : 1;
  /* True if there was multiple COMDAT bodies merged by lto-symtab.  */
  unsigned merged_comdat : 1;
  /* True if this def was merged with extern inlines.  */
  unsigned merged_extern_inline : 1;
  /* True if function was created to be executed in parallel.  */
  unsigned parallelized_function : 1;
  /* True if function is part split out by ipa-split.  */
  unsigned split_part : 1;
  /* True if the function appears as possible target of indirect call.  */
  unsigned indirect_call_target : 1;
  /* Set when function is visible in current compilation unit only and
     its address is never taken.  */
  unsigned local : 1;
  /* False when there is something makes versioning impossible.  */
  unsigned versionable : 1;
  /* False when function calling convention and signature cannot be changed.
     This is the case when __builtin_apply_args is used.  */
  unsigned can_change_signature : 1;
  /* True when the function has been originally extern inline, but it is
     redefined now.  */
  unsigned redefined_extern_inline : 1;
  /* True if the function may enter serial irrevocable mode.  */
  unsigned tm_may_enter_irr : 1;
  /* True if this was a clone created by ipa-cp.  */
  unsigned ipcp_clone : 1;
  /* True if the function should only be emitted if it is used.  This flag
     is set for local SIMD clones when they are created and cleared if the
     vectorizer uses them.  */
  unsigned gc_candidate : 1;
  /* Set if the function is called by an IFUNC resolver.  */
  unsigned called_by_ifunc_resolver : 1;
  /* True if the function contains unresolved OpenMP metadirectives.  */
  unsigned has_omp_variant_constructs : 1;

private:

  /* Summary id that is recycled.  */
  int m_summary_id;

  /* Worker for call_for_symbol_and_aliases.  */
  bool call_for_symbol_and_aliases_1 (bool (*callback) (cgraph_node *,
						        void *),
				      void *data, bool include_overwritable);
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

struct varpool_node;

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

/* Context of polymorphic call. It represent information about the type of
   instance that may reach the call.  This is used by ipa-devirt walkers of the
   type inheritance graph.  */

class GTY(()) ipa_polymorphic_call_context {
public:
  /* The called object appears in an object of type OUTER_TYPE
     at offset OFFSET.  When information is not 100% reliable, we
     use SPECULATIVE_OUTER_TYPE and SPECULATIVE_OFFSET. */
  HOST_WIDE_INT offset;
  HOST_WIDE_INT speculative_offset;
  tree outer_type;
  tree speculative_outer_type;
  /* True if outer object may be in construction or destruction.  */
  unsigned maybe_in_construction : 1;
  /* True if outer object may be of derived type.  */
  unsigned maybe_derived_type : 1;
  /* True if speculative outer object may be of derived type.  We always
     speculate that construction does not happen.  */
  unsigned speculative_maybe_derived_type : 1;
  /* True if the context is invalid and all calls should be redirected
     to BUILTIN_UNREACHABLE.  */
  unsigned invalid : 1;
  /* True if the outer type is dynamic.  */
  unsigned dynamic : 1;

  /* Build empty "I know nothing" context.  */
  ipa_polymorphic_call_context ();
  /* Build polymorphic call context for indirect call E.  */
  ipa_polymorphic_call_context (cgraph_edge *e);
  /* Build polymorphic call context for IP invariant CST.
     If specified, OTR_TYPE specify the type of polymorphic call
     that takes CST+OFFSET as a parameter.  */
  ipa_polymorphic_call_context (tree cst, tree otr_type = NULL,
				HOST_WIDE_INT offset = 0);
  /* Build context for pointer REF contained in FNDECL at statement STMT.
     if INSTANCE is non-NULL, return pointer to the object described by
     the context.  */
  ipa_polymorphic_call_context (tree fndecl, tree ref, gimple *stmt,
				tree *instance = NULL);

  /* Look for vtable stores or constructor calls to work out dynamic type
     of memory location.  */
  bool get_dynamic_type (tree, tree, tree, gimple *, unsigned *);

  /* Make context non-speculative.  */
  void clear_speculation ();

  /* Produce context specifying all derived types of OTR_TYPE.  If OTR_TYPE is
     NULL, the context is set to dummy "I know nothing" setting.  */
  void clear_outer_type (tree otr_type = NULL);

  /* Walk container types and modify context to point to actual class
     containing OTR_TYPE (if non-NULL) as base class.
     Return true if resulting context is valid.

     When CONSIDER_PLACEMENT_NEW is false, reject contexts that may be made
     valid only via allocation of new polymorphic type inside by means
     of placement new.

     When CONSIDER_BASES is false, only look for actual fields, not base types
     of TYPE.  */
  bool restrict_to_inner_class (tree otr_type,
				bool consider_placement_new = true,
				bool consider_bases = true);

  /* Adjust all offsets in contexts by given number of bits.  */
  void offset_by (HOST_WIDE_INT);
  /* Use when we cannot track dynamic type change.  This speculatively assume
     type change is not happening.  */
  void possible_dynamic_type_change (bool, tree otr_type = NULL);
  /* Assume that both THIS and a given context is valid and strengthen THIS
     if possible.  Return true if any strengthening was made.
     If actual type the context is being used in is known, OTR_TYPE should be
     set accordingly. This improves quality of combined result.  */
  bool combine_with (ipa_polymorphic_call_context, tree otr_type = NULL);
  bool meet_with (ipa_polymorphic_call_context, tree otr_type = NULL);

  /* Return TRUE if context is fully useless.  */
  bool useless_p () const;
  /* Return TRUE if this context conveys the same information as X.  */
  bool equal_to (const ipa_polymorphic_call_context &x) const;

  /* Dump human readable context to F.  If NEWLINE is true, it will be
     terminated by a newline.  */
  void dump (FILE *f, bool newline = true) const;
  void DEBUG_FUNCTION debug () const;

  /* LTO streaming.  */
  void stream_out (struct output_block *) const;
  void stream_in (class lto_input_block *, class data_in *data_in);

private:
  bool combine_speculation_with (tree, HOST_WIDE_INT, bool, tree);
  bool meet_speculation_with (tree, HOST_WIDE_INT, bool, tree);
  void set_by_decl (tree, HOST_WIDE_INT);
  bool set_by_invariant (tree, tree, HOST_WIDE_INT);
  bool speculation_consistent_p (tree, HOST_WIDE_INT, bool, tree) const;
  void make_speculative (tree otr_type = NULL);
};

/* Structure containing additional information about an indirect call.  */

class GTY(()) cgraph_indirect_call_info
{
public:
  /* When agg_content is set, an offset where the call pointer is located
     within the aggregate.  */
  HOST_WIDE_INT offset;
  /* Context of the polymorphic call; use only when POLYMORPHIC flag is set.  */
  ipa_polymorphic_call_context context;
  /* OBJ_TYPE_REF_TOKEN of a polymorphic call (if polymorphic is set).  */
  HOST_WIDE_INT otr_token;
  /* Type of the object from OBJ_TYPE_REF_OBJECT. */
  tree otr_type;
  /* Index of the parameter that is called.  */
  int param_index;
  /* ECF flags determined from the caller.  */
  int ecf_flags;

  /* Number of speculative call targets, it's less than GCOV_TOPN_VALUES.  */
  unsigned num_speculative_call_targets : 16;

  /* Set when the call is a virtual call with the parameter being the
     associated object pointer rather than a simple direct call.  */
  unsigned polymorphic : 1;
  /* Set when the call is a call of a pointer loaded from contents of an
     aggregate at offset.  */
  unsigned agg_contents : 1;
  /* Set when this is a call through a member pointer.  */
  unsigned member_ptr : 1;
  /* When the agg_contents bit is set, this one determines whether the
     destination is loaded from a parameter passed by reference. */
  unsigned by_ref : 1;
  /* When the agg_contents bit is set, this one determines whether we can
     deduce from the function body that the loaded value from the reference is
     never modified between the invocation of the function and the load
     point.  */
  unsigned guaranteed_unmodified : 1;
  /* For polymorphic calls this specify whether the virtual table pointer
     may have changed in between function entry and the call.  */
  unsigned vptr_changed : 1;
};

class GTY((chain_next ("%h.next_caller"), chain_prev ("%h.prev_caller"),
	   for_user)) cgraph_edge
{
public:
  friend struct cgraph_node;
  friend class symbol_table;

  /* Remove EDGE from the cgraph.  */
  static void remove (cgraph_edge *edge);

  /* Change field call_stmt of edge E to NEW_STMT.  If UPDATE_SPECULATIVE and E
     is any component of speculative edge, then update all components.
     Speculations can be resolved in the process and EDGE can be removed and
     deallocated.  Return the edge that now represents the call.  */
  static cgraph_edge *set_call_stmt (cgraph_edge *e, gcall *new_stmt,
				     bool update_speculative = true);

  /* Redirect callee of the edge to N.  The function does not update underlying
     call expression.  */
  void redirect_callee (cgraph_node *n);

  /* If the edge does not lead to a thunk, simply redirect it to N.  Otherwise
     create one or more equivalent thunks for N and redirect E to the first in
     the chain.  Note that it is then necessary to call
     n->expand_all_artificial_thunks once all callers are redirected.  */
  void redirect_callee_duplicating_thunks (cgraph_node *n);

  /* Make an indirect edge with an unknown callee an ordinary edge leading to
     CALLEE.  Speculations can be resolved in the process and EDGE can be
     removed and deallocated.  Return the edge that now represents the
     call.  */
  static cgraph_edge *make_direct (cgraph_edge *edge, cgraph_node *callee);

  /* Turn edge into speculative call calling N2. Update
     the profile so the direct call is taken COUNT times
     with FREQUENCY.  speculative_id is used to link direct calls with their
     corresponding IPA_REF_ADDR references when representing speculative calls.
   */
  cgraph_edge *make_speculative (cgraph_node *n2, profile_count direct_count,
				 unsigned int speculative_id = 0);

  /* Speculative call consists of an indirect edge and one or more
     direct edge+ref pairs.  Speculative will expand to the following sequence:

     if (call_dest == target1)		// reference to target1
	target1 ();			// direct call to target1
     else if (call_dest == target2)	// reference to targt2
	target2 ();			// direct call to target2
     else
	call_dest ();			// indirect call

     Before the expansion we will have indirect call and the direct call+ref
     pairs all linked to single statement.

     Note that ref may point to different symbol than the corresponding call
     becuase the speculated edge may have been optimized (redirected to
     a clone) or inlined.

     Given an edge which is part of speculative call, return the first
     direct call edge in the speculative call sequence.

     In the example above called on any cgraph edge in the sequence it will
     return direct call to target1.  */
  cgraph_edge *first_speculative_call_target ();

  /* Return next speculative call target or NULL if there is none.
     All targets are required to form an interval in the callee list.

     In example above, if called on call to target1 it will return call to
     target2.  */
  cgraph_edge *next_speculative_call_target ()
  {
    cgraph_edge *e = this;
    gcc_checking_assert (speculative && callee);

    if (e->next_callee && e->next_callee->speculative
	&& e->next_callee->call_stmt == e->call_stmt
	&& e->next_callee->lto_stmt_uid == e->lto_stmt_uid)
      return e->next_callee;
    return NULL;
  }

  /* When called on any edge in the speculative call return the (unique)
     indirect call edge in the speculative call sequence.  */
  cgraph_edge *speculative_call_indirect_edge ()
  {
    gcc_checking_assert (speculative);
    if (!callee)
      return this;
    for (cgraph_edge *e2 = caller->indirect_calls;
	 true; e2 = e2->next_callee)
      if (e2->speculative
	  && call_stmt == e2->call_stmt
	  && lto_stmt_uid == e2->lto_stmt_uid)
	return e2;
  }

  /* When called on any edge in speculative call and when given any target
     of ref which is speculated to it returns the corresponding direct call.

     In example above if called on function target2 it will return call to
     target2.  */
  cgraph_edge *speculative_call_for_target (cgraph_node *);

  /* Return REF corresponding to direct call in the specualtive call
     sequence.  */
  ipa_ref *speculative_call_target_ref ()
  {
    ipa_ref *ref;

    gcc_checking_assert (speculative);
    for (unsigned int i = 0; caller->iterate_reference (i, ref); i++)
      if (ref->speculative && ref->speculative_id == speculative_id
	  && ref->stmt == (gimple *)call_stmt
	  && ref->lto_stmt_uid == lto_stmt_uid)
	return ref;
    gcc_unreachable ();
  }

  /* Speculative call edge turned out to be direct call to CALLEE_DECL.  Remove
     the speculative call sequence and return edge representing the call, the
     original EDGE can be removed and deallocated.  It is up to caller to
     redirect the call as appropriate.  Return the edge that now represents the
     call.

     For "speculative" indirect call that contains multiple "speculative"
     targets (i.e. edge->indirect_info->num_speculative_call_targets > 1),
     decrease the count and only remove current direct edge.

     If no speculative direct call left to the speculative indirect call, remove
     the speculative of both the indirect call and corresponding direct edge.

     It is up to caller to iteratively resolve each "speculative" direct call
     and redirect the call as appropriate.  */
  static cgraph_edge *resolve_speculation (cgraph_edge *edge,
					   tree callee_decl = NULL);

  /* If necessary, change the function declaration in the call statement
     associated with edge E so that it corresponds to the edge callee.
     Speculations can be resolved in the process and EDGE can be removed and
     deallocated.

     The edge could be one of speculative direct call generated from speculative
     indirect call.  In this circumstance, decrease the speculative targets
     count (i.e. num_speculative_call_targets) and redirect call stmt to the
     corresponding i-th target.  If no speculative direct call left to the
     speculative indirect call, remove "speculative" of the indirect call and
     also redirect stmt to it's final direct target.

     When called from within tree-inline, KILLED_SSAs has to contain the
     pointer to killed_new_ssa_names within the copy_body_data structure and
     SSAs discovered to be useless (if LHS is removed) will be added to it,
     otherwise it needs to be NULL.

     It is up to caller to iteratively transform each "speculative"
     direct call as appropriate.  */
  static gimple *redirect_call_stmt_to_callee (cgraph_edge *e,
					       hash_set <tree>
					       *killed_ssas = nullptr);

  /* Create clone of edge in the node N represented
     by CALL_EXPR the callgraph.  */
  cgraph_edge * clone (cgraph_node *n, gcall *call_stmt, unsigned stmt_uid,
		       profile_count num, profile_count den,
		       bool update_original);

  /* Verify edge count and frequency.  */
  bool verify_count ();

  /* Return true when call of edge cannot lead to return from caller
     and thus it is safe to ignore its side effects for IPA analysis
     when computing side effects of the caller.  */
  bool cannot_lead_to_return_p (void);

  /* Return true when the edge represents a direct recursion.  */
  bool recursive_p (void);

  /* Return true if the edge may be considered hot after scalling its count.  */
  bool maybe_hot_p ();

  /* Return true if the edge may be considered hot after scalling its count
     (i.e. assume that optimization would reduce runtime for callee,
      possibly significantly).  */
  bool maybe_hot_p (sreal scale);

  /* Get unique identifier of the edge.  */
  inline int get_uid ()
  {
    return m_uid;
  }

  /* Get summary id of the edge.  */
  inline int get_summary_id ()
  {
    return m_summary_id;
  }

  /* Rebuild cgraph edges for current function node.  This needs to be run after
     passes that don't update the cgraph.  */
  static unsigned int rebuild_edges (void);

  /* Rebuild cgraph references for current function node.  This needs to be run
     after passes that don't update the cgraph.  */
  static void rebuild_references (void);

  /* During LTO stream in this can be used to check whether call can possibly
     be internal to the current translation unit.  */
  bool possibly_call_in_translation_unit_p (void);

  /* Return num_speculative_targets of this edge.  */
  int num_speculative_call_targets_p (void);

  /* Expected number of executions: calculated in profile.cc.  */
  profile_count count;
  cgraph_node *caller;
  cgraph_node *callee;
  cgraph_edge *prev_caller;
  cgraph_edge *next_caller;
  cgraph_edge *prev_callee;
  cgraph_edge *next_callee;
  gcall *call_stmt;
  /* Additional information about an indirect call.  Not cleared when an edge
     becomes direct.  */
  cgraph_indirect_call_info *indirect_info;
  void *GTY ((skip (""))) aux;
  /* When equal to CIF_OK, inline this call.  Otherwise, points to the
     explanation why function was not inlined.  */
  enum cgraph_inline_failed_t inline_failed;
  /* The stmt_uid of call_stmt.  This is used by LTO to recover the call_stmt
     when the function is serialized in.  */
  unsigned int lto_stmt_uid;
  /* speculative id is used to link direct calls with their corresponding
     IPA_REF_ADDR references when representing speculative calls.  */
  unsigned int speculative_id : 16;
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
     3) a IPA_REF_ADDR reference to expected_fn.

     Optimizers may later redirect direct call to clone, so 1) and 3)
     do not need to necessarily agree with destination.  */
  unsigned int speculative : 1;
  /* Set to true when caller is a constructor or destructor of polymorphic
     type.  */
  unsigned in_polymorphic_cdtor : 1;

  /* Return true if call must bind to current definition.  */
  bool binds_to_current_def_p ();

  /* Expected frequency of executions within the function.
     When set to CGRAPH_FREQ_BASE, the edge is expected to be called once
     per function call.  The range is 0 to CGRAPH_FREQ_MAX.  */
  int frequency ();

  /* Expected frequency of executions within the function.  */
  sreal sreal_frequency ();
private:
  /* Unique id of the edge.  */
  int m_uid;

  /* Summary id that is recycled.  */
  int m_summary_id;

  /* Remove the edge from the list of the callers of the callee.  */
  void remove_caller (void);

  /* Remove the edge from the list of the callees of the caller.  */
  void remove_callee (void);

  /* Set callee N of call graph edge and add it to the corresponding set of
     callers. */
  void set_callee (cgraph_node *n);

  /* Output flags of edge to a file F.  */
  void dump_edge_flags (FILE *f);

  /* Dump edge to stderr.  */
  void DEBUG_FUNCTION debug (void);

  /* Verify that call graph edge corresponds to DECL from the associated
     statement.  Return true if the verification should fail.  */
  bool verify_corresponds_to_fndecl (tree decl);
};

#define CGRAPH_FREQ_BASE 1000
#define CGRAPH_FREQ_MAX 100000

/* The varpool data structure.
   Each static variable decl has assigned varpool_node.  */

struct GTY((tag ("SYMTAB_VARIABLE"))) varpool_node : public symtab_node
{
  /* Constructor.  */
  explicit varpool_node ()
    : symtab_node (SYMTAB_VARIABLE), output (0), dynamically_initialized (0),
      tls_model (TLS_MODEL_NONE), used_by_single_function (0)
  {}

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
  availability get_availability (symtab_node *ref = NULL);

  /* When doing LTO, read variable's constructor from disk if
     it is not already present.  */
  tree get_constructor (void);

  /* Return true if variable has constructor that can be used for folding.  */
  bool ctor_useable_for_folding_p (void);

  /* For given variable pool node, walk the alias chain to return the function
     the variable is alias of. Do not walk through thunks.
     When AVAILABILITY is non-NULL, get minimal availability in the chain.
     When REF is non-NULL, assume that reference happens in symbol REF
     when determining the availability.  */
  inline varpool_node *ultimate_alias_target
    (availability *availability = NULL, symtab_node *ref = NULL);

  /* Return node that alias is aliasing.  */
  inline varpool_node *get_alias_target (void);

  /* Output one variable, if necessary.  Return whether we output it.  */
  bool assemble_decl (void);

  /* For variables in named sections make sure get_variable_section
     is called before we switch to those sections.  Then section
     conflicts between read-only and read-only requiring relocations
     sections can be resolved.  */
  void finalize_named_section_flags (void);

  /* Call callback on varpool symbol and aliases associated to varpool symbol.
     When INCLUDE_OVERWRITABLE is false, overwritable aliases and thunks are
     skipped. */
  bool call_for_symbol_and_aliases (bool (*callback) (varpool_node *, void *),
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

  /* Add the variable DECL to the varpool.
     Unlike finalize_decl function is intended to be used
     by middle end and allows insertion of new variable at arbitrary point
     of compilation.  */
  static void add (tree decl);

  /* Return varpool node for given symbol and check it is a function. */
  static inline varpool_node *get (const_tree decl);

  /* Mark DECL as finalized.  By finalizing the declaration, frontend instruct
     the middle end to output the variable to asm file, if needed or externally
     visible.  */
  static void finalize_decl (tree decl);

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
     This is computed by ipa_single_use pass and used by late optimizations
     in places where optimization would be valid for local static variable
     if we did not do any inter-procedural code movement.  */
  unsigned used_by_single_function : 1;

private:
  /* Assemble thunks and aliases associated to varpool node.  */
  void assemble_aliases (void);

  /* Worker for call_for_node_and_aliases.  */
  bool call_for_symbol_and_aliases_1 (bool (*callback) (varpool_node *, void *),
				      void *data,
				      bool include_overwritable);
};

/* Every top level asm statement is put into a asm_node.  */

struct GTY(()) asm_node {
  /* Next asm node.  */
  asm_node *next;
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

/* Report whether or not THIS symtab node is a variable, aka varpool_node.  */

template <>
template <>
inline bool
is_a_helper <varpool_node *>::test (symtab_node *p)
{
  return p && p->type == SYMTAB_VARIABLE;
}

typedef void (*cgraph_edge_hook)(cgraph_edge *, void *);
typedef void (*cgraph_node_hook)(cgraph_node *, void *);
typedef void (*varpool_node_hook)(varpool_node *, void *);
typedef void (*cgraph_2edge_hook)(cgraph_edge *, cgraph_edge *, void *);
typedef void (*cgraph_2node_hook)(cgraph_node *, cgraph_node *, void *);

struct cgraph_edge_hook_list;
struct cgraph_node_hook_list;
struct varpool_node_hook_list;
struct cgraph_2edge_hook_list;
struct cgraph_2node_hook_list;

/* Map from a symbol to initialization/finalization priorities.  */
struct GTY(()) symbol_priority_map {
  priority_type init;
  priority_type fini;
};

enum symtab_state
{
  /* Frontend is parsing and finalizing functions.  */
  PARSING,
  /* Callgraph is being constructed.  It is safe to add new functions.  */
  CONSTRUCTION,
  /* Callgraph is being streamed-in at LTO time.  */
  LTO_STREAMING,
  /* Callgraph is built and early IPA passes are being run.  */
  IPA,
  /* Callgraph is built and all functions are transformed to SSA form.  */
  IPA_SSA,
  /* All inline decisions are done; it is now possible to remove extern inline
     functions and virtual call targets.  */
  IPA_SSA_AFTER_INLINING,
  /* Functions are now ordered and being passed to RTL expanders.  */
  EXPANSION,
  /* All cgraph expansion is done.  */
  FINISHED
};

struct asmname_hasher : ggc_ptr_hash <symtab_node>
{
  typedef const_tree compare_type;

  static hashval_t hash (symtab_node *n);
  static bool equal (symtab_node *n, const_tree t);
};

/* Core summaries maintained about symbols.  */

struct thunk_info;
template <class T> class function_summary;
typedef function_summary <thunk_info *> thunk_summary;

struct clone_info;
template <class T> class function_summary;
typedef function_summary <clone_info *> clone_summary;

class GTY((tag ("SYMTAB"))) symbol_table
{
public:
  friend struct symtab_node;
  friend struct cgraph_node;
  friend struct cgraph_edge;

  symbol_table ():
  cgraph_count (0), cgraph_max_uid (1), cgraph_max_summary_id (0),
  edges_count (0), edges_max_uid (1), edges_max_summary_id (0),
  cgraph_released_summary_ids (), edge_released_summary_ids (),
  nodes (NULL), asmnodes (NULL), asm_last_node (NULL),
  order (0), max_unit (0), global_info_ready (false), state (PARSING),
  function_flags_ready (false), cpp_implicit_aliases_done (false),
  section_hash (NULL), assembler_name_hash (NULL), init_priority_hash (NULL),
  dump_file (NULL), ipa_clones_dump_file (NULL), cloned_nodes (),
  m_thunks (NULL), m_clones (NULL),
  m_first_edge_removal_hook (NULL), m_first_cgraph_removal_hook (NULL),
  m_first_edge_duplicated_hook (NULL), m_first_cgraph_duplicated_hook (NULL),
  m_first_cgraph_insertion_hook (NULL), m_first_varpool_insertion_hook (NULL),
  m_first_varpool_removal_hook (NULL)
  {
  }

  /* Initialize callgraph dump file.  */
  void initialize (void);

  /* Register a top-level asm statement ASM_STR.  */
  inline asm_node *finalize_toplevel_asm (tree asm_str);

  /* Analyze the whole compilation unit once it is parsed completely.  */
  void finalize_compilation_unit (void);

  /* C++ frontend produce same body aliases all over the place, even before PCH
     gets streamed out. It relies on us linking the aliases with their function
     in order to do the fixups, but ipa-ref is not PCH safe.  Consequently we
     first produce aliases without links, but once C++ FE is sure it won't
     stream PCH we build the links via this function.  */
  void process_same_body_aliases (void);

  /* Perform simple optimizations based on callgraph.  */
  void compile (void);

  /* Process CGRAPH_NEW_FUNCTIONS and perform actions necessary to add these
     functions into callgraph in a way so they look like ordinary reachable
     functions inserted into callgraph already at construction time.  */
  void process_new_functions (void);

  /* Register a symbol NODE.  */
  inline void register_symbol (symtab_node *node);

  inline void
  clear_asm_symbols (void)
  {
    asmnodes = NULL;
    asm_last_node = NULL;
  }

  /* Perform reachability analysis and reclaim all unreachable nodes.  */
  bool remove_unreachable_nodes (FILE *file);

  /* Optimization of function bodies might've rendered some variables as
     unnecessary so we want to avoid these from being compiled.  Re-do
     reachability starting from variables that are either externally visible
     or was referred from the asm output routines.  */
  void remove_unreferenced_decls (void);

  /* Unregister a symbol NODE.  */
  inline void unregister (symtab_node *node);

  /* Allocate new callgraph node and insert it into basic data structures.  */
  cgraph_node *create_empty (void);

  /* Release a callgraph NODE.  */
  void release_symbol (cgraph_node *node);

  /* Output all variables enqueued to be assembled.  */
  bool output_variables (void);

  /* Weakrefs may be associated to external decls and thus not output
     at expansion time.  Emit all necessary aliases.  */
  void output_weakrefs (void);

  /* Return first static symbol with definition.  */
  inline symtab_node *first_symbol (void);

  /* Return first assembler symbol.  */
  inline asm_node *
  first_asm_symbol (void)
  {
    return asmnodes;
  }

  /* Return first static symbol with definition.  */
  inline symtab_node *first_defined_symbol (void);

  /* Return first variable.  */
  inline varpool_node *first_variable (void);

  /* Return next variable after NODE.  */
  inline varpool_node *next_variable (varpool_node *node);

  /* Return first static variable with initializer.  */
  inline varpool_node *first_static_initializer (void);

  /* Return next static variable with initializer after NODE.  */
  inline varpool_node *next_static_initializer (varpool_node *node);

  /* Return first static variable with definition.  */
  inline varpool_node *first_defined_variable (void);

  /* Return next static variable with definition after NODE.  */
  inline varpool_node *next_defined_variable (varpool_node *node);

  /* Return first function with body defined.  */
  inline cgraph_node *first_defined_function (void);

  /* Return next function with body defined after NODE.  */
  inline cgraph_node *next_defined_function (cgraph_node *node);

  /* Return first function.  */
  inline cgraph_node *first_function (void);

  /* Return next function.  */
  inline cgraph_node *next_function (cgraph_node *node);

  /* Return first function with body defined.  */
  cgraph_node *first_function_with_gimple_body (void);

  /* Return next reachable static variable with initializer after NODE.  */
  inline cgraph_node *next_function_with_gimple_body (cgraph_node *node);

  /* Register HOOK to be called with DATA on each removed edge.  */
  cgraph_edge_hook_list *add_edge_removal_hook (cgraph_edge_hook hook,
						void *data);

  /* Remove ENTRY from the list of hooks called on removing edges.  */
  void remove_edge_removal_hook (cgraph_edge_hook_list *entry);

  /* Register HOOK to be called with DATA on each removed node.  */
  cgraph_node_hook_list *add_cgraph_removal_hook (cgraph_node_hook hook,
						  void *data);

  /* Remove ENTRY from the list of hooks called on removing nodes.  */
  void remove_cgraph_removal_hook (cgraph_node_hook_list *entry);

  /* Register HOOK to be called with DATA on each removed node.  */
  varpool_node_hook_list *add_varpool_removal_hook (varpool_node_hook hook,
						    void *data);

  /* Remove ENTRY from the list of hooks called on removing nodes.  */
  void remove_varpool_removal_hook (varpool_node_hook_list *entry);

  /* Register HOOK to be called with DATA on each inserted node.  */
  cgraph_node_hook_list *add_cgraph_insertion_hook (cgraph_node_hook hook,
						    void *data);

  /* Remove ENTRY from the list of hooks called on inserted nodes.  */
  void remove_cgraph_insertion_hook (cgraph_node_hook_list *entry);

  /* Register HOOK to be called with DATA on each inserted node.  */
  varpool_node_hook_list *add_varpool_insertion_hook (varpool_node_hook hook,
						      void *data);

  /* Remove ENTRY from the list of hooks called on inserted nodes.  */
  void remove_varpool_insertion_hook (varpool_node_hook_list *entry);

  /* Register HOOK to be called with DATA on each duplicated edge.  */
  cgraph_2edge_hook_list *add_edge_duplication_hook (cgraph_2edge_hook hook,
						     void *data);
  /* Remove ENTRY from the list of hooks called on duplicating edges.  */
  void remove_edge_duplication_hook (cgraph_2edge_hook_list *entry);

  /* Register HOOK to be called with DATA on each duplicated node.  */
  cgraph_2node_hook_list *add_cgraph_duplication_hook (cgraph_2node_hook hook,
						       void *data);

  /* Remove ENTRY from the list of hooks called on duplicating nodes.  */
  void remove_cgraph_duplication_hook (cgraph_2node_hook_list *entry);

  /* Call all edge removal hooks.  */
  void call_edge_removal_hooks (cgraph_edge *e);

  /* Call all node insertion hooks.  */
  void call_cgraph_insertion_hooks (cgraph_node *node);

  /* Call all node removal hooks.  */
  void call_cgraph_removal_hooks (cgraph_node *node);

  /* Call all node duplication hooks.  */
  void call_cgraph_duplication_hooks (cgraph_node *node, cgraph_node *node2);

  /* Call all edge duplication hooks.  */
  void call_edge_duplication_hooks (cgraph_edge *cs1, cgraph_edge *cs2);

  /* Call all node removal hooks.  */
  void call_varpool_removal_hooks (varpool_node *node);

  /* Call all node insertion hooks.  */
  void call_varpool_insertion_hooks (varpool_node *node);

  /* Arrange node to be first in its entry of assembler_name_hash.  */
  void symtab_prevail_in_asm_name_hash (symtab_node *node);

  /* Initialize asm name hash unless.  */
  void symtab_initialize_asm_name_hash (void);

  /* Set the DECL_ASSEMBLER_NAME and update symtab hashtables.  */
  void change_decl_assembler_name (tree decl, tree name);

  /* Dump symbol table to F.  */
  void dump (FILE *f);

  /* Dump symbol table to F in graphviz format.  */
  void dump_graphviz (FILE *f);

  /* Dump symbol table to stderr.  */
  void DEBUG_FUNCTION debug (void);

  /* Assign a new summary ID for the callgraph NODE.  */
  inline int assign_summary_id (cgraph_node *node)
  {
    if (!cgraph_released_summary_ids.is_empty ())
      node->m_summary_id = cgraph_released_summary_ids.pop ();
    else
      node->m_summary_id = cgraph_max_summary_id++;

    return node->m_summary_id;
  }

  /* Assign a new summary ID for the callgraph EDGE.  */
  inline int assign_summary_id (cgraph_edge *edge)
  {
    if (!edge_released_summary_ids.is_empty ())
      edge->m_summary_id = edge_released_summary_ids.pop ();
    else
      edge->m_summary_id = edges_max_summary_id++;

    return edge->m_summary_id;
  }

  /* Return true if assembler names NAME1 and NAME2 leads to the same symbol
     name.  */
  static bool assembler_names_equal_p (const char *name1, const char *name2);

  int cgraph_count;
  int cgraph_max_uid;
  int cgraph_max_summary_id;

  int edges_count;
  int edges_max_uid;
  int edges_max_summary_id;

  /* Vector of released summary IDS for cgraph nodes.  */
  vec<int> GTY ((skip)) cgraph_released_summary_ids;

  /* Vector of released summary IDS for cgraph nodes.  */
  vec<int> GTY ((skip)) edge_released_summary_ids;

  /* Return symbol used to separate symbol name from suffix.  */
  static char symbol_suffix_separator ();

  symtab_node* GTY(()) nodes;
  asm_node* GTY(()) asmnodes;
  asm_node* GTY(()) asm_last_node;

  /* The order index of the next symtab node to be created.  This is
     used so that we can sort the cgraph nodes in order by when we saw
     them, to support -fno-toplevel-reorder.  */
  int order;

  /* Maximal unit ID used.  */
  int max_unit;

  /* Set when whole unit has been analyzed so we can access global info.  */
  bool global_info_ready;
  /* What state callgraph is in right now.  */
  enum symtab_state state;
  /* Set when the cgraph is fully build and the basic flags are computed.  */
  bool function_flags_ready;

  bool cpp_implicit_aliases_done;

  /* Hash table used to hold sections.  */
  hash_table<section_name_hasher> *GTY(()) section_hash;

  /* Hash table used to convert assembler names into nodes.  */
  hash_table<asmname_hasher> *assembler_name_hash;

  /* Hash table used to hold init priorities.  */
  hash_map<symtab_node *, symbol_priority_map> *init_priority_hash;

  FILE* GTY ((skip)) dump_file;

  FILE* GTY ((skip)) ipa_clones_dump_file;

  hash_set <const cgraph_node *> GTY ((skip)) cloned_nodes;

  /* Thunk annotations.  */
  thunk_summary *m_thunks;

  /* Virtual clone annotations.  */
  clone_summary *m_clones;

private:
  /* Allocate a cgraph_edge structure and fill it with data according to the
     parameters of which only CALLEE can be NULL (when creating an indirect
     call edge).  CLONING_P should be set if properties that are copied from an
     original edge should not be calculated.  */
  cgraph_edge *create_edge (cgraph_node *caller, cgraph_node *callee,
			    gcall *call_stmt, profile_count count,
			    bool indir_unknown_callee, bool cloning_p);

  /* Put the edge onto the free list.  */
  void free_edge (cgraph_edge *e);

  /* Insert NODE to assembler name hash.  */
  void insert_to_assembler_name_hash (symtab_node *node, bool with_clones);

  /* Remove NODE from assembler name hash.  */
  void unlink_from_assembler_name_hash (symtab_node *node, bool with_clones);

  /* Hash asmnames ignoring the user specified marks.  */
  static hashval_t decl_assembler_name_hash (const_tree asmname);

  /* Compare ASMNAME with the DECL_ASSEMBLER_NAME of DECL.  */
  static bool decl_assembler_name_equal (tree decl, const_tree asmname);

  friend struct asmname_hasher;

  /* List of hooks triggered when an edge is removed.  */
  cgraph_edge_hook_list * GTY((skip)) m_first_edge_removal_hook;
  /* List of hooks trigger_red when a cgraph node is removed.  */
  cgraph_node_hook_list * GTY((skip)) m_first_cgraph_removal_hook;
  /* List of hooks triggered when an edge is duplicated.  */
  cgraph_2edge_hook_list * GTY((skip)) m_first_edge_duplicated_hook;
  /* List of hooks triggered when a node is duplicated.  */
  cgraph_2node_hook_list * GTY((skip)) m_first_cgraph_duplicated_hook;
  /* List of hooks triggered when an function is inserted.  */
  cgraph_node_hook_list * GTY((skip)) m_first_cgraph_insertion_hook;
  /* List of hooks triggered when an variable is inserted.  */
  varpool_node_hook_list * GTY((skip)) m_first_varpool_insertion_hook;
  /* List of hooks triggered when a node is removed.  */
  varpool_node_hook_list * GTY((skip)) m_first_varpool_removal_hook;
};

extern GTY(()) symbol_table *symtab;

extern vec<cgraph_node *> cgraph_new_nodes;

inline hashval_t
asmname_hasher::hash (symtab_node *n)
{
  return symbol_table::decl_assembler_name_hash
    (DECL_ASSEMBLER_NAME (n->decl));
}

inline bool
asmname_hasher::equal (symtab_node *n, const_tree t)
{
  return symbol_table::decl_assembler_name_equal (n->decl, t);
}

/* In cgraph.cc  */
void cgraph_cc_finalize (void);
void release_function_body (tree);
cgraph_indirect_call_info *cgraph_allocate_init_indirect_info (void);

void cgraph_update_edges_for_call_stmt (gimple *, tree, gimple *);
bool cgraph_function_possibly_inlined_p (tree);

const char* cgraph_inline_failed_string (cgraph_inline_failed_t);
cgraph_inline_failed_type_t cgraph_inline_failed_type (cgraph_inline_failed_t);

/* In cgraphunit.cc  */
void cgraphunit_cc_finalize (void);
int tp_first_run_node_cmp (const void *pa, const void *pb);

/* In symtab-thunks.cc  */
void symtab_thunks_cc_finalize (void);

/*  Initialize datastructures so DECL is a function in lowered gimple form.
    IN_SSA is true if the gimple is in SSA.  */
basic_block init_lowered_empty_function (tree, bool, profile_count);

tree thunk_adjust (gimple_stmt_iterator *, tree, bool, HOST_WIDE_INT, tree,
		   HOST_WIDE_INT);
/* In cgraphclones.cc  */

tree clone_function_name_numbered (const char *name, const char *suffix);
tree clone_function_name_numbered (tree decl, const char *suffix);
tree clone_function_name (const char *name, const char *suffix,
			  unsigned long number);
tree clone_function_name (tree decl, const char *suffix,
			  unsigned long number);
tree clone_function_name (tree decl, const char *suffix);

void tree_function_versioning (tree, tree, vec<ipa_replace_map *, va_gc> *,
			       ipa_param_adjustments *,
			       bool, bitmap, basic_block);

void dump_callgraph_transformation (const cgraph_node *original,
				    const cgraph_node *clone,
				    const char *suffix);
void set_new_clone_decl_and_node_flags (cgraph_node *new_node);
/* In cgraphbuild.cc  */
int compute_call_stmt_bb_frequency (tree, basic_block bb);
void record_references_in_initializer (tree, bool);

/* In ipa.cc  */
void cgraph_build_static_cdtor (char which, tree body, int priority);
bool ipa_discover_variable_flags (void);

/* In varpool.cc  */
tree ctor_for_folding (tree);

/* In ipa-inline-analysis.cc  */
void initialize_inline_failed (struct cgraph_edge *);
bool speculation_useful_p (struct cgraph_edge *e, bool anticipate_inlining);

/* Return true when the symbol is real symbol, i.e. it is not inline clone
   or abstract function kept for debug info purposes only.  */
inline bool
symtab_node::real_symbol_p (void)
{
  cgraph_node *cnode;

  if (DECL_ABSTRACT_P (decl))
    return false;
  if (transparent_alias && definition)
    return false;
  if (!is_a <cgraph_node *> (this))
    return true;
  cnode = dyn_cast <cgraph_node *> (this);
  if (cnode->inlined_to)
    return false;
  return true;
}

/* Return true if DECL should have entry in symbol table if used.
   Those are functions and static & external variables.  */

inline bool
decl_in_symtab_p (const_tree decl)
{
  return (TREE_CODE (decl) == FUNCTION_DECL
	  || (VAR_P (decl)
	      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl))));
}

inline bool
symtab_node::in_same_comdat_group_p (symtab_node *target)
{
  symtab_node *source = this;

  if (cgraph_node *cn = dyn_cast <cgraph_node *> (target))
    {
      if (cn->inlined_to)
	source = cn->inlined_to;
    }
  if (cgraph_node *cn = dyn_cast <cgraph_node *> (target))
    {
      if (cn->inlined_to)
	target = cn->inlined_to;
    }

  return source->get_comdat_group () == target->get_comdat_group ();
}

/* Return node that alias is aliasing.  */

inline symtab_node *
symtab_node::get_alias_target (void)
{
  ipa_ref *ref = NULL;
  iterate_reference (0, ref);
  gcc_checking_assert (ref->use == IPA_REF_ALIAS);
  return ref->referred;
}

/* Return the DECL (or identifier) that alias is aliasing.  Unlike the above,
   this works whether or not the alias has been analyzed already.  */

inline tree
symtab_node::get_alias_target_tree ()
{
  if (alias_target)
    return alias_target;
  return get_alias_target ()->decl;
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

/* Iterates I-th reference in the list, REF is also set.  */

inline ipa_ref *
symtab_node::iterate_reference (unsigned i, ipa_ref *&ref)
{
  ref_list.references.iterate (i, &ref);

  return ref;
}

/* Iterates I-th referring item in the list, REF is also set.  */

inline ipa_ref *
symtab_node::iterate_referring (unsigned i, ipa_ref *&ref)
{
  ref_list.referring.iterate (i, &ref);

  return ref;
}

/* Iterates I-th referring alias item in the list, REF is also set.  */

inline ipa_ref *
symtab_node::iterate_direct_aliases (unsigned i, ipa_ref *&ref)
{
  ref_list.referring.iterate (i, &ref);

  if (ref && ref->use != IPA_REF_ALIAS)
    return NULL;

  return ref;
}

/* Return true if list contains an alias.  */

inline bool
symtab_node::has_aliases_p (void)
{
  ipa_ref *ref = NULL;

  return (iterate_direct_aliases (0, ref) != NULL);
}

/* Return true when RESOLUTION indicate that linker will use
   the symbol from non-LTO object files.  */

inline bool
resolution_used_from_other_file_p (enum ld_plugin_symbol_resolution resolution)
{
  return (resolution == LDPR_PREVAILING_DEF
	  || resolution == LDPR_PREEMPTED_REG
	  || resolution == LDPR_RESOLVED_EXEC
	  || resolution == LDPR_RESOLVED_DYN);
}

/* Return true when symtab_node is known to be used from other (non-LTO)
   object file. Known only when doing LTO via linker plugin.  */

inline bool
symtab_node::used_from_object_file_p (void)
{
  if (!TREE_PUBLIC (decl) || DECL_EXTERNAL (decl))
    return false;
  if (resolution_used_from_other_file_p (resolution))
    return true;
  return false;
}

/* Return varpool node for given symbol and check it is a function. */

inline varpool_node *
varpool_node::get (const_tree decl)
{
  gcc_checking_assert (TREE_CODE (decl) == VAR_DECL);
  return dyn_cast<varpool_node *> (symtab_node::get (decl));
}

/* Register a symbol NODE.  */

inline void
symbol_table::register_symbol (symtab_node *node)
{
  node->next = nodes;
  node->previous = NULL;

  if (nodes)
    nodes->previous = node;
  nodes = node;

  nodes->m_uid = cgraph_max_uid++;

  if (node->order == -1)
    node->order = order++;
}

/* Register a top-level asm statement ASM_STR.  */

asm_node *
symbol_table::finalize_toplevel_asm (tree asm_str)
{
  asm_node *node;

  node = ggc_cleared_alloc<asm_node> ();
  node->asm_str = asm_str;
  node->order = order++;
  node->next = NULL;

  if (asmnodes == NULL)
    asmnodes = node;
  else
    asm_last_node->next = node;

  asm_last_node = node;
  return node;
}

/* Unregister a symbol NODE.  */
inline void
symbol_table::unregister (symtab_node *node)
{
  if (node->previous)
    node->previous->next = node->next;
  else
    nodes = node->next;

  if (node->next)
    node->next->previous = node->previous;

  node->next = NULL;
  node->previous = NULL;
}

/* Release a callgraph NODE with UID and put in to the list of free nodes.  */

inline void
symbol_table::release_symbol (cgraph_node *node)
{
  cgraph_count--;
  if (node->m_summary_id != -1)
    cgraph_released_summary_ids.safe_push (node->m_summary_id);
  ggc_free (node);
}

/* Return first static symbol with definition.  */
inline symtab_node *
symbol_table::first_symbol (void)
{
  return nodes;
}

/* Walk all symbols.  */
#define FOR_EACH_SYMBOL(node) \
   for ((node) = symtab->first_symbol (); (node); (node) = (node)->next)

/* Return first static symbol with definition.  */
inline symtab_node *
symbol_table::first_defined_symbol (void)
{
  symtab_node *node;

  for (node = nodes; node; node = node->next)
    if (node->definition)
      return node;

  return NULL;
}

/* Walk all symbols with definitions in current unit.  */
#define FOR_EACH_DEFINED_SYMBOL(node) \
   for ((node) = symtab->first_defined_symbol (); (node); \
	(node) = node->next_defined_symbol ())

/* Return first variable.  */
inline varpool_node *
symbol_table::first_variable (void)
{
  symtab_node *node;
  for (node = nodes; node; node = node->next)
    if (varpool_node *vnode = dyn_cast <varpool_node *> (node))
      return vnode;
  return NULL;
}

/* Return next variable after NODE.  */
inline varpool_node *
symbol_table::next_variable (varpool_node *node)
{
  symtab_node *node1 = node->next;
  for (; node1; node1 = node1->next)
    if (varpool_node *vnode1 = dyn_cast <varpool_node *> (node1))
      return vnode1;
  return NULL;
}
/* Walk all variables.  */
#define FOR_EACH_VARIABLE(node) \
   for ((node) = symtab->first_variable (); \
        (node); \
	(node) = symtab->next_variable ((node)))

/* Return first static variable with initializer.  */
inline varpool_node *
symbol_table::first_static_initializer (void)
{
  symtab_node *node;
  for (node = nodes; node; node = node->next)
    {
      varpool_node *vnode = dyn_cast <varpool_node *> (node);
      if (vnode && DECL_INITIAL (node->decl))
	return vnode;
    }
  return NULL;
}

/* Return next static variable with initializer after NODE.  */
inline varpool_node *
symbol_table::next_static_initializer (varpool_node *node)
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
   for ((node) = symtab->first_static_initializer (); (node); \
	(node) = symtab->next_static_initializer (node))

/* Return first static variable with definition.  */
inline varpool_node *
symbol_table::first_defined_variable (void)
{
  symtab_node *node;
  for (node = nodes; node; node = node->next)
    {
      varpool_node *vnode = dyn_cast <varpool_node *> (node);
      if (vnode && vnode->definition)
	return vnode;
    }
  return NULL;
}

/* Return next static variable with definition after NODE.  */
inline varpool_node *
symbol_table::next_defined_variable (varpool_node *node)
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
   for ((node) = symtab->first_defined_variable (); (node); \
	(node) = symtab->next_defined_variable (node))

/* Return first function with body defined.  */
inline cgraph_node *
symbol_table::first_defined_function (void)
{
  symtab_node *node;
  for (node = nodes; node; node = node->next)
    {
      cgraph_node *cn = dyn_cast <cgraph_node *> (node);
      if (cn && cn->definition)
	return cn;
    }
  return NULL;
}

/* Return next function with body defined after NODE.  */
inline cgraph_node *
symbol_table::next_defined_function (cgraph_node *node)
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
   for ((node) = symtab->first_defined_function (); (node); \
	(node) = symtab->next_defined_function ((node)))

/* Return first function.  */
inline cgraph_node *
symbol_table::first_function (void)
{
  symtab_node *node;
  for (node = nodes; node; node = node->next)
    if (cgraph_node *cn = dyn_cast <cgraph_node *> (node))
      return cn;
  return NULL;
}

/* Return next function.  */
inline cgraph_node *
symbol_table::next_function (cgraph_node *node)
{
  symtab_node *node1 = node->next;
  for (; node1; node1 = node1->next)
    if (cgraph_node *cn1 = dyn_cast <cgraph_node *> (node1))
      return cn1;
  return NULL;
}

/* Return first function with body defined.  */
inline cgraph_node *
symbol_table::first_function_with_gimple_body (void)
{
  symtab_node *node;
  for (node = nodes; node; node = node->next)
    {
      cgraph_node *cn = dyn_cast <cgraph_node *> (node);
      if (cn && cn->has_gimple_body_p ())
	return cn;
    }
  return NULL;
}

/* Return next reachable static variable with initializer after NODE.  */
inline cgraph_node *
symbol_table::next_function_with_gimple_body (cgraph_node *node)
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

/* Walk all functions.  */
#define FOR_EACH_FUNCTION(node) \
   for ((node) = symtab->first_function (); (node); \
	(node) = symtab->next_function ((node)))

/* Return true when callgraph node is a function with Gimple body defined
   in current unit.  Functions can also be define externally or they
   can be thunks with no Gimple representation.

   Note that at WPA stage, the function body may not be present in memory.  */

inline bool
cgraph_node::has_gimple_body_p (void)
{
  return definition && !thunk && !alias;
}

/* Walk all functions with body defined.  */
#define FOR_EACH_FUNCTION_WITH_GIMPLE_BODY(node) \
   for ((node) = symtab->first_function_with_gimple_body (); (node); \
	(node) = symtab->next_function_with_gimple_body (node))

/* Uniquize all constants that appear in memory.
   Each constant in memory thus far output is recorded
   in `const_desc_table'.  */

struct GTY((for_user)) constant_descriptor_tree {
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
  gcc_assert (!inlined_to);
  return (!force_output && !address_taken
	  && !ifunc_resolver
	  && !used_from_other_partition
	  && !DECL_VIRTUAL_P (decl)
	  && !DECL_STATIC_CONSTRUCTOR (decl)
	  && !DECL_STATIC_DESTRUCTOR (decl)
	  && !used_from_object_file_p ()
	  && !externally_visible);
}

/* Return true when function can be removed from callgraph
   if all direct calls are eliminated.  */

inline bool
cgraph_node::can_remove_if_no_direct_calls_and_refs_p (void)
{
  gcc_checking_assert (!inlined_to);
  /* Extern inlines can always go, we will use the external definition.  */
  if (DECL_EXTERNAL (decl))
    return true;
  /* When function is needed, we cannot remove it.  */
  if (force_output || used_from_other_partition)
    return false;
  if (DECL_STATIC_CONSTRUCTOR (decl)
      || DECL_STATIC_DESTRUCTOR (decl))
    return false;
  /* Only COMDAT functions can be removed if externally visible.  */
  if (externally_visible
      && ((!DECL_COMDAT (decl) || ifunc_resolver)
	  || forced_by_abi
	  || used_from_object_file_p ()))
    return false;
  return true;
}

/* Verify cgraph, if consistency checking is enabled.  */

inline void
cgraph_node::checking_verify_cgraph_nodes (void)
{
  if (flag_checking)
    cgraph_node::verify_cgraph_nodes ();
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

struct tree_descriptor_hasher : ggc_ptr_hash<constant_descriptor_tree>
{
  static hashval_t hash (constant_descriptor_tree *);
  static bool equal (constant_descriptor_tree *, constant_descriptor_tree *);
};

/* Constant pool accessor function.  */
hash_table<tree_descriptor_hasher> *constant_pool_htab (void);

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

/* Walk the alias chain to return the symbol NODE is alias of.
   If NODE is not an alias, return NODE.
   When AVAILABILITY is non-NULL, get minimal availability in the chain.
   When REF is non-NULL, assume that reference happens in symbol REF
   when determining the availability.  */

inline symtab_node *
symtab_node::ultimate_alias_target (enum availability *availability,
				    symtab_node *ref)
{
  if (!alias)
    {
      if (availability)
	*availability = get_availability (ref);
      return this;
    }

  return ultimate_alias_target_1 (availability, ref);
}

/* Given function symbol, walk the alias chain to return the function node
   is alias of. Do not walk through thunks.
   When AVAILABILITY is non-NULL, get minimal availability in the chain.
   When REF is non-NULL, assume that reference happens in symbol REF
   when determining the availability.  */

inline cgraph_node *
cgraph_node::ultimate_alias_target (enum availability *availability,
				    symtab_node *ref)
{
  cgraph_node *n = dyn_cast <cgraph_node *>
    (symtab_node::ultimate_alias_target (availability, ref));
  if (!n && availability)
    *availability = AVAIL_NOT_AVAILABLE;
  return n;
}

/* For given variable pool node, walk the alias chain to return the function
   the variable is alias of. Do not walk through thunks.
   When AVAILABILITY is non-NULL, get minimal availability in the chain.
   When REF is non-NULL, assume that reference happens in symbol REF
   when determining the availability.  */

inline varpool_node *
varpool_node::ultimate_alias_target (availability *availability,
				     symtab_node *ref)
{
  varpool_node *n = dyn_cast <varpool_node *>
    (symtab_node::ultimate_alias_target (availability, ref));

  if (!n && availability)
    *availability = AVAIL_NOT_AVAILABLE;
  return n;
}

/* Set callee N of call graph edge and add it to the corresponding set of
   callers. */

inline void
cgraph_edge::set_callee (cgraph_node *n)
{
  prev_caller = NULL;
  if (n->callers)
    n->callers->prev_caller = this;
  next_caller = n->callers;
  n->callers = this;
  callee = n;
}

/* Return true when the edge represents a direct recursion.  */

inline bool
cgraph_edge::recursive_p (void)
{
  cgraph_node *c = callee->ultimate_alias_target ();
  if (caller->inlined_to)
    return caller->inlined_to->decl == c->decl;
  else
    return caller->decl == c->decl;
}

/* Remove the edge from the list of the callers of the callee.  */

inline void
cgraph_edge::remove_callee (void)
{
  gcc_assert (!indirect_unknown_callee);
  if (prev_caller)
    prev_caller->next_caller = next_caller;
  if (next_caller)
    next_caller->prev_caller = prev_caller;
  if (!prev_caller)
    callee->callers = next_caller;
}

/* Return true if call must bind to current definition.  */

inline bool
cgraph_edge::binds_to_current_def_p ()
{
  if (callee)
    return callee->binds_to_current_def_p (caller);
  else
    return false;
}

/* Expected frequency of executions within the function.
   When set to CGRAPH_FREQ_BASE, the edge is expected to be called once
   per function call.  The range is 0 to CGRAPH_FREQ_MAX.  */

inline int
cgraph_edge::frequency ()
{
  return count.to_cgraph_frequency (caller->inlined_to
				    ? caller->inlined_to->count
				    : caller->count);
}


/* Return true if the TM_CLONE bit is set for a given FNDECL.  */
inline bool
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
  gcc_checking_assert (!inlined_to);
}

/* Return true if function should be optimized for size.  */

inline enum optimize_size_level
cgraph_node::optimize_for_size_p (void)
{
  if (opt_for_fn (decl, optimize_size))
    return OPTIMIZE_SIZE_MAX;
  if (count == profile_count::zero ())
    return OPTIMIZE_SIZE_MAX;
  if (frequency == NODE_FREQUENCY_UNLIKELY_EXECUTED)
    return OPTIMIZE_SIZE_BALANCED;
  else
    return OPTIMIZE_SIZE_NO;
}

/* Return symtab_node for NODE or create one if it is not present
   in symtab.  */

inline symtab_node *
symtab_node::get_create (tree node)
{
  if (VAR_P (node))
    return varpool_node::get_create (node);
  else
    return cgraph_node::get_create (node);
}

/* Return availability of NODE when referenced from REF.  */

inline enum availability
symtab_node::get_availability (symtab_node *ref)
{
  if (is_a <cgraph_node *> (this))
    return dyn_cast <cgraph_node *> (this)->get_availability (ref);
  else
    return dyn_cast <varpool_node *> (this)->get_availability (ref);
}

/* Call callback on symtab node and aliases associated to this node.
   When INCLUDE_OVERWRITABLE is false, overwritable symbols are skipped. */

inline bool
symtab_node::call_for_symbol_and_aliases (bool (*callback) (symtab_node *,
							    void *),
					  void *data,
					  bool include_overwritable)
{
  if (include_overwritable
      || get_availability () > AVAIL_INTERPOSABLE)
    {
      if (callback (this, data))
        return true;
    }
  if (has_aliases_p ())
    return call_for_symbol_and_aliases_1 (callback, data, include_overwritable);
  return false;
}

/* Call callback on function and aliases associated to the function.
   When INCLUDE_OVERWRITABLE is false, overwritable symbols are
   skipped.  */

inline bool
cgraph_node::call_for_symbol_and_aliases (bool (*callback) (cgraph_node *,
							    void *),
					  void *data,
					  bool include_overwritable)
{
  if (include_overwritable
      || get_availability () > AVAIL_INTERPOSABLE)
    {
      if (callback (this, data))
        return true;
    }
  if (has_aliases_p ())
    return call_for_symbol_and_aliases_1 (callback, data, include_overwritable);
  return false;
}

/* Call callback on varpool symbol and aliases associated to varpool symbol.
   When INCLUDE_OVERWRITABLE is false, overwritable symbols are
   skipped. */

inline bool
varpool_node::call_for_symbol_and_aliases (bool (*callback) (varpool_node *,
							     void *),
					   void *data,
					   bool include_overwritable)
{
  if (include_overwritable
      || get_availability () > AVAIL_INTERPOSABLE)
    {
      if (callback (this, data))
        return true;
    }
  if (has_aliases_p ())
    return call_for_symbol_and_aliases_1 (callback, data, include_overwritable);
  return false;
}

/* Return true if reference may be used in address compare.  */

inline bool
ipa_ref::address_matters_p ()
{
  if (use != IPA_REF_ADDR)
    return false;
  /* Addresses taken from virtual tables are never compared.  */
  if (is_a <varpool_node *> (referring)
      && DECL_VIRTUAL_P (referring->decl))
    return false;
  return referred->address_can_be_compared_p ();
}

/* Build polymorphic call context for indirect call E.  */

inline
ipa_polymorphic_call_context::ipa_polymorphic_call_context (cgraph_edge *e)
{
  gcc_checking_assert (e->indirect_info->polymorphic);
  *this = e->indirect_info->context;
}

/* Build empty "I know nothing" context.  */

inline
ipa_polymorphic_call_context::ipa_polymorphic_call_context ()
{
  clear_speculation ();
  clear_outer_type ();
  invalid = false;
}

/* Make context non-speculative.  */

inline void
ipa_polymorphic_call_context::clear_speculation ()
{
  speculative_outer_type = NULL;
  speculative_offset = 0;
  speculative_maybe_derived_type = false;
}

/* Produce context specifying all derived types of OTR_TYPE.  If OTR_TYPE is
   NULL, the context is set to dummy "I know nothing" setting.  */

inline void
ipa_polymorphic_call_context::clear_outer_type (tree otr_type)
{
  outer_type = otr_type ? TYPE_MAIN_VARIANT (otr_type) : NULL;
  offset = 0;
  maybe_derived_type = true;
  maybe_in_construction = true;
  dynamic = true;
}

/* Adjust all offsets in contexts by OFF bits.  */

inline void
ipa_polymorphic_call_context::offset_by (HOST_WIDE_INT off)
{
  if (outer_type)
    offset += off;
  if (speculative_outer_type)
    speculative_offset += off;
}

/* Return TRUE if context is fully useless.  */

inline bool
ipa_polymorphic_call_context::useless_p () const
{
  return (!outer_type && !speculative_outer_type);
}

/* When using fprintf (or similar), problems can arise with
   transient generated strings.  Many string-generation APIs
   only support one result being alive at once (e.g. by
   returning a pointer to a statically-allocated buffer).

   If there is more than one generated string within one
   fprintf call: the first string gets evicted or overwritten
   by the second, before fprintf is fully evaluated.
   See e.g. PR/53136.

   This function provides a workaround for this, by providing
   a simple way to create copies of these transient strings,
   without the need to have explicit cleanup:

       fprintf (dumpfile, "string 1: %s string 2:%s\n",
                xstrdup_for_dump (EXPR_1),
                xstrdup_for_dump (EXPR_2));

   This is actually a simple wrapper around ggc_strdup, but
   the name documents the intent.  We require that no GC can occur
   within the fprintf call.  */

inline const char *
xstrdup_for_dump (const char *transient_str)
{
  return ggc_strdup (transient_str);
}

/* During LTO stream-in this predicate can be used to check whether node
   in question prevails in the linking to save some memory usage.  */
inline bool
symtab_node::prevailing_p (void)
{
  return definition && ((!TREE_PUBLIC (decl) && !DECL_EXTERNAL (decl))
			 || previous_sharing_asm_name == NULL);
}

extern GTY(()) symbol_table *saved_symtab;

#if CHECKING_P

namespace selftest {

/* An RAII-style class for use in selftests for temporarily using a different
   symbol_table, so that such tests can be isolated from each other.  */

class symbol_table_test
{
 public:
  /* Constructor.  Override "symtab".  */
  symbol_table_test ();

  /* Destructor.  Restore the saved_symtab.  */
  ~symbol_table_test ();
};

} // namespace selftest

#endif /* CHECKING_P */

#endif  /* GCC_CGRAPH_H  */
