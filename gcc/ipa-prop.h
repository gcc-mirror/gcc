/* Interprocedural analyses.
   Copyright (C) 2005, 2007 Free Software Foundation, Inc.

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

#ifndef IPA_PROP_H
#define IPA_PROP_H

#include "tree.h"

/* The following definitions and interfaces are used by
   interprocedural analyses.  */

/* A jump function for a callsite represents the values passed as actual 
   arguments of the callsite. There are three main types of values :
   Formal - the caller's formal parameter is passed as an actual argument.
   Constant - a constant is passed as an actual argument.
   Unknown - neither of the above.
   Integer and real constants are represented as IPA_CONST and Fortran
   constants are represented as IPA_CONST_REF.  */
enum jump_func_type
{
  IPA_UNKNOWN,
  IPA_CONST,
  IPA_CONST_REF,
  IPA_PASS_THROUGH
};

/* All formal parameters in the program have a lattice associated with it
   computed by the interprocedural stage of IPCP.
   There are three main values of the lattice:
   TOP - unknown.
   BOTTOM - non constant.
   CONSTANT_TYPE - constant value.
   Cval of formal f will have a constant value if all callsites to this
   function have the same constant value passed to f.
   Integer and real constants are represented as IPA_CONST and Fortran
   constants are represented as IPA_CONST_REF.  */
enum ipa_lattice_type
{
  IPA_BOTTOM,
  IPA_CONST_VALUE,
  IPA_CONST_VALUE_REF,
  IPA_TOP
};

/* Represents a value of a jump function.
   value represents a constant.
   formal_id is used only in jump function context and represents 
   pass-through parameter (the formal of caller is passed as argument).  */
union jump_func_value
{
  unsigned int formal_id;
  tree constant;
};

/* A jump function for a callsite represents the values passed as actual 
   arguments of the callsite. See enum jump_func_type for the various 
   types of jump functions supported.  */
struct ipa_jump_func
{
  enum jump_func_type type;
  union jump_func_value value;
};

/* All formal parameters in the program have a cval computed by 
   the interprocedural stage of IPCP. See enum ipa_lattice_type for
   the various types of lattices supported */
struct ipcp_lattice
{
  enum ipa_lattice_type type;
  tree constant;
};

/* Represent which DECL tree (or reference to such tree)
   will be replaced by another tree while versioning.  */
struct ipa_replace_map
{
  /* The tree that will be replaced.  */
  tree old_tree;
  /* The new (replacing) tree.  */ 
  tree new_tree;
  /* True when a substitution should be done, false otherwise.  */
  bool replace_p;
  /* True when we replace a reference to old_tree.  */
  bool ref_p;
};

/* Return the field in cgraph_node/cgraph_edge struct that points
   to ipa_node_params/ipa_edge_args struct.  */
#define IPA_NODE_REF(MT) ((struct ipa_node_params *)(MT)->aux)
#define IPA_EDGE_REF(EDGE) ((struct ipa_edge_args *)(EDGE)->aux)
/* This macro checks validity of index returned by
   ipa_get_param_decl_index function.  */
#define IS_VALID_JUMP_FUNC_INDEX(I) ((I) != -1)

/* ipa_node_params stores information related to formal parameters of functions
   and some other information for interprocedural passes that operate on
   parameters (such as ipa-cp).  */

struct ipa_node_params
{
  /* Number of formal parameters of this function.  When set to 0,
     this functions's parameters would not be analyzed by the different
     stages of IPA CP.  */
  int param_count;
  /* Array of lattices.  */
  struct ipcp_lattice *ipcp_lattices;
  /* Mapping each parameter to its PARM_DECL tree.  */
  tree *param_decls;
  /* Indicating which parameter is modified in its function.  */
  bool *modified_flags;
  /* Only for versioned nodes this field would not be NULL,
     it points to the node that IPA cp cloned from.  */
  struct cgraph_node *ipcp_orig_node;
  /* Meaningful only for original functions.  Expresses the
     ratio between the direct calls and sum of all invocations of 
     this function (given by profiling info).  It is used to calculate 
     the profiling information of the original function and the versioned
     one.  */
  gcov_type count_scale;

  /* Whether this fynction is called with variable number of actual
     arguments.  */
  unsigned called_with_var_arguments : 1;
};

/* ipa_node_params access functions.  Please use these to access fields that
   are or will be shared among various passes.  */

/* Set the number of formal parameters. */
static inline void
ipa_set_param_count (struct ipa_node_params *info, int count)
{
  info->param_count = count;
}

/* Return the number of formal parameters. */
static inline int
ipa_get_param_count (struct ipa_node_params *info)
{
  return info->param_count;
}

/* Returns the declaration of ith param of the corresponding node.  Note there
   is no setter function as this array is built just once using
   ipa_create_param_decls_array. */
static inline tree
ipa_get_ith_param (struct ipa_node_params *info, int i)
{
  return info->param_decls[i];
}

/* Returns the modification flag corresponding o the ith paramterer.  Note
   there is no setter method as the goal is to set all flags when building the
   array in ipa_detect_param_modifications.  */
static inline bool
ipa_is_ith_param_modified (struct ipa_node_params *info, int i)
{
  return info->modified_flags[i];
}

/* Flag this node as having callers with variable number of arguments.  */
static inline void
ipa_set_called_with_variable_arg (struct ipa_node_params *info)
{
  info->called_with_var_arguments = 1;
}

/* Have we detected this node was called with variable number of arguments? */
static inline bool
ipa_is_called_with_var_arguments (struct ipa_node_params *info)
{
  return info->called_with_var_arguments;
}



/* ipa_edge_args stores information related to a callsite and particularly
   its arguments. It is pointed to by a field in the
   callsite's corresponding cgraph_edge.  */
struct ipa_edge_args
{
  /* Number of actual arguments in this callsite.  When set to 0,
     this callsite's parameters would not be analyzed by the different
     stages of IPA CP.  */
  int argument_count;
  /* Array of the callsite's jump function of each parameter.  */
  struct ipa_jump_func *jump_functions;
};

/* ipa_edge_args access functions.  Please use these to access fields that
   are or will be shared among various passes.  */

/* Set the number of actual arguments. */
static inline void
ipa_set_cs_argument_count (struct ipa_edge_args *args, int count)
{
  args->argument_count = count;
}

/* Return the number of actual arguments. */
static inline int
ipa_get_cs_argument_count (struct ipa_edge_args *args)
{
  return args->argument_count;
}

/* Returns a pointer to the jump function for the ith argument.  Please note
   there is no setter function as jump functions are all set up in
   ipa_compute_jump_functions. */
static inline struct ipa_jump_func *
ipa_get_ith_jump_func (struct ipa_edge_args *args, int i)
{
  return &args->jump_functions[i];
}

/* A function list element.  It is used to create a temporary worklist used in
   the propagation stage of IPCP. (can be used for more IPA optimizations)  */
struct ipa_func_list
{
  struct cgraph_node *node;
  struct ipa_func_list *next;
};

/* ipa_func_list interface.  */
struct ipa_func_list *ipa_init_func_list (void);
void ipa_push_func_to_list (struct ipa_func_list **, struct cgraph_node *);
struct cgraph_node *ipa_pop_func_from_list (struct ipa_func_list **);

/* Callsite related calculations.  */
void ipa_compute_jump_functions (struct cgraph_edge *);
void ipa_count_arguments (struct cgraph_edge *);

/* Function parameters related computations.  */
void ipa_count_formal_params (struct cgraph_node *);
void ipa_create_param_decls_array (struct cgraph_node *);
void ipa_detect_param_modifications (struct cgraph_node *);

/* Creating and freeing ipa_node_params and ipa_edge_args.  */
void ipa_create_node_params (struct cgraph_node *);
void ipa_free_all_node_params (void);
void ipa_create_all_node_params (void);
void ipa_create_all_edge_args (void);
void ipa_free_all_edge_args (void);

/* Debugging interface.  */
void ipa_print_all_tree_maps (FILE *);
void ipa_print_all_params_modified (FILE *);

#endif /* IPA_PROP_H */
