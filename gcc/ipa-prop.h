/* Interprocedural analyses.
   Copyright (C) 2005, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

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
#include "vec.h"
#include "cgraph.h"
#include "gimple.h"

/* The following definitions and interfaces are used by
   interprocedural analyses or parameters.  */

/* ipa-prop.c stuff (ipa-cp, indirect inlining):  */

/* A jump function for a callsite represents the values passed as actual
   arguments of the callsite. There are three main types of values :

   Pass-through - the caller's formal parameter is passed as an actual
                  argument, possibly one simple operation performed on it.
   Constant     - a constant (is_gimple_ip_invariant)is passed as an actual
                  argument.
   Unknown      - neither of the above.

   IPA_JF_CONST_MEMBER_PTR stands for C++ member pointers, it is a special
   constant in this regard.  Other constants are represented with IPA_JF_CONST.

   IPA_JF_ANCESTOR is a special pass-through jump function, which means that
   the result is an address of a part of the object pointed to by the formal
   parameter to which the function refers.  It is mainly intended to represent
   getting addresses of of ancestor fields in C++
   (e.g. &this_1(D)->D.1766.D.1756).  Note that if the original pointer is
   NULL, ancestor jump function must behave like a simple pass-through.

   Other pass-through functions can either simply pass on an unchanged formal
   parameter or can apply one simple binary operation to it (such jump
   functions are called polynomial).

   IPA_JF_KNOWN_TYPE is a special type of an "unknown" function that applies
   only to pointer parameters.  It means that even though we cannot prove that
   the passed value is an interprocedural constant, we still know the exact
   type of the containing object which may be valuable for devirtualization.

   Jump functions are computed in ipa-prop.c by function
   update_call_notes_after_inlining.  Some information can be lost and jump
   functions degraded accordingly when inlining, see
   update_call_notes_after_inlining in the same file.  */

enum jump_func_type
{
  IPA_JF_UNKNOWN = 0,  /* newly allocated and zeroed jump functions default */
  IPA_JF_KNOWN_TYPE,        /* represented by field base_binfo */
  IPA_JF_CONST,             /* represented by field costant */
  IPA_JF_CONST_MEMBER_PTR,  /* represented by field member_cst */
  IPA_JF_PASS_THROUGH,	    /* represented by field pass_through */
  IPA_JF_ANCESTOR	    /* represented by field ancestor */
};

/* Structure holding data required to describe a pass-through jump function.  */

struct GTY(()) ipa_pass_through_data
{
  /* If an operation is to be performed on the original parameter, this is the
     second (constant) operand.  */
  tree operand;
  /* Number of the caller's formal parameter being passed.  */
  int formal_id;
  /* Operation that is performed on the argument before it is passed on.
     NOP_EXPR means no operation.  Otherwise oper must be a simple binary
     arithmetic operation where the caller's parameter is the first operand and
     operand field from this structure is the second one.  */
  enum tree_code operation;
};

/* Structure holding data required to describe an ancestor pass-through
   jump function.  */

struct GTY(()) ipa_ancestor_jf_data
{
  /* Offset of the field representing the ancestor.  */
  HOST_WIDE_INT offset;
  /* TYpe of the result.  */
  tree type;
  /* Number of the caller's formal parameter being passed.  */
  int formal_id;
};

/* Structure holding a C++ member pointer constant.  Holds a pointer to the
   method and delta offset.  */
struct GTY(()) ipa_member_ptr_cst
{
  tree pfn;
  tree delta;
};

/* A jump function for a callsite represents the values passed as actual
   arguments of the callsite. See enum jump_func_type for the various
   types of jump functions supported.  */
struct GTY (()) ipa_jump_func
{
  enum jump_func_type type;
  /* Represents a value of a jump function.  pass_through is used only in jump
     function context.  constant represents the actual constant in constant jump
     functions and member_cst holds constant c++ member functions.  */
  union jump_func_value
  {
    tree GTY ((tag ("IPA_JF_KNOWN_TYPE"))) base_binfo;
    tree GTY ((tag ("IPA_JF_CONST"))) constant;
    struct ipa_member_ptr_cst GTY ((tag ("IPA_JF_CONST_MEMBER_PTR"))) member_cst;
    struct ipa_pass_through_data GTY ((tag ("IPA_JF_PASS_THROUGH"))) pass_through;
    struct ipa_ancestor_jf_data GTY ((tag ("IPA_JF_ANCESTOR"))) ancestor;
  } GTY ((desc ("%1.type"))) value;
};

/* All formal parameters in the program have a lattice associated with it
   computed by the interprocedural stage of IPCP.
   There are three main values of the lattice:
   IPA_TOP - unknown,
   IPA_BOTTOM - variable,
   IPA_CONST_VALUE - simple scalar constant,

   We also use this type to propagate types accross the call graph for the
   purpose of devirtualization.  In that case, IPA_CONST_VALUE denotes a known
   type, rather than a constant.  */
enum ipa_lattice_type
{
  IPA_BOTTOM,
  IPA_CONST_VALUE,
  IPA_TOP
};

/* All formal parameters in the program have a cval computed by
   the interprocedural stage of IPCP. See enum ipa_lattice_type for
   the various types of lattices supported */
struct ipcp_lattice
{
  enum ipa_lattice_type type;
  tree constant;
};

/* Structure describing a single formal parameter.  */
struct ipa_param_descriptor
{
  /* IPA-CP lattice.  */
  struct ipcp_lattice ipcp_lattice;
  /* PARAM_DECL of this parameter.  */
  tree decl;
  /* Vector of BINFOs of types that this argument might encounter.  NULL
     basically means a top value, bottom is marked by the cannot_devirtualize
     flag below.*/
  VEC (tree, heap) *types;
  /* The parameter is used.  */
  unsigned used : 1;
  /* Set when parameter type cannot be used for devirtualization.  */
  unsigned cannot_devirtualize : 1;
};

/* ipa_node_params stores information related to formal parameters of functions
   and some other information for interprocedural passes that operate on
   parameters (such as ipa-cp).  */
struct ipa_node_params
{
  /* Number of formal parameters of this function.  When set to 0, this
     function's parameters would not be analyzed by IPA CP.  */
  int param_count;
  /* Whether this function is called with variable number of actual
     arguments.  */
  unsigned called_with_var_arguments : 1;
  /* Whether the param uses analysis has already been performed.  */
  unsigned uses_analysis_done : 1;
  /* Whether the function is enqueued in an ipa_func_list.  */
  unsigned node_enqueued : 1;
  /* Pointer to an array of structures describing individual formal
     parameters.  */
  struct ipa_param_descriptor *params;
  /* Only for versioned nodes this field would not be NULL,
     it points to the node that IPA cp cloned from.  */
  struct cgraph_node *ipcp_orig_node;
  /* Meaningful only for original functions.  Expresses the
     ratio between the direct calls and sum of all invocations of
     this function (given by profiling info).  It is used to calculate
     the profiling information of the original function and the versioned
     one.  */
  gcov_type count_scale;
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

/* Return the declaration of Ith formal parameter of the function corresponding
   to INFO.  Note there is no setter function as this array is built just once
   using ipa_initialize_node_params. */

static inline tree
ipa_get_param (struct ipa_node_params *info, int i)
{
  return info->params[i].decl;
}

/* Return the used flag corresponding to the Ith formal parameter of
   the function associated with INFO.  */

static inline bool
ipa_is_param_used (struct ipa_node_params *info, int i)
{
  return info->params[i].used;
}

/* Return the cannot_devirtualize flag corresponding to the Ith formal
   parameter of the function associated with INFO.  The corresponding function
   to set the flag is ipa_set_param_cannot_devirtualize.  */

static inline bool
ipa_param_cannot_devirtualize_p (struct ipa_node_params *info, int i)
{
  return info->params[i].cannot_devirtualize;
}

/* Return true iff the vector of possible types of the Ith formal parameter of
   the function associated with INFO is empty.  */

static inline bool
ipa_param_types_vec_empty (struct ipa_node_params *info, int i)
{
  return info->params[i].types == NULL;
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



/* ipa_edge_args stores information related to a callsite and particularly its
   arguments.  It can be accessed by the IPA_EDGE_REF macro.  */
typedef struct GTY(()) ipa_edge_args
{
  /* Number of actual arguments in this callsite.  When set to 0,
     this callsite's parameters would not be analyzed by the different
     stages of IPA CP.  */
  int argument_count;
  /* Array of the callsite's jump function of each parameter.  */
  struct ipa_jump_func GTY ((length ("%h.argument_count"))) *jump_functions;
} ipa_edge_args_t;

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

/* Vectors need to have typedefs of structures.  */
typedef struct ipa_node_params ipa_node_params_t;

/* Types of vectors holding the infos.  */
DEF_VEC_O (ipa_node_params_t);
DEF_VEC_ALLOC_O (ipa_node_params_t, heap);
DEF_VEC_O (ipa_edge_args_t);
DEF_VEC_ALLOC_O (ipa_edge_args_t, gc);

/* Vector where the parameter infos are actually stored. */
extern VEC (ipa_node_params_t, heap) *ipa_node_params_vector;
/* Vector where the parameter infos are actually stored. */
extern GTY(()) VEC (ipa_edge_args_t, gc) *ipa_edge_args_vector;

/* Return the associated parameter/argument info corresponding to the given
   node/edge.  */
#define IPA_NODE_REF(NODE) (VEC_index (ipa_node_params_t, \
				       ipa_node_params_vector, (NODE)->uid))
#define IPA_EDGE_REF(EDGE) (VEC_index (ipa_edge_args_t, \
				       ipa_edge_args_vector, (EDGE)->uid))
/* This macro checks validity of index returned by
   ipa_get_param_decl_index function.  */
#define IS_VALID_JUMP_FUNC_INDEX(I) ((I) != -1)

/* Creating and freeing ipa_node_params and ipa_edge_args.  */
void ipa_create_all_node_params (void);
void ipa_create_all_edge_args (void);
void ipa_free_edge_args_substructures (struct ipa_edge_args *);
void ipa_free_node_params_substructures (struct ipa_node_params *);
void ipa_free_all_node_params (void);
void ipa_free_all_edge_args (void);
void ipa_create_all_structures_for_iinln (void);
void ipa_free_all_structures_after_ipa_cp (void);
void ipa_free_all_structures_after_iinln (void);
void ipa_register_cgraph_hooks (void);

/* This function ensures the array of node param infos is big enough to
   accommodate a structure for all nodes and reallocates it if not.  */

static inline void
ipa_check_create_node_params (void)
{
  if (!ipa_node_params_vector)
    ipa_node_params_vector = VEC_alloc (ipa_node_params_t, heap,
					cgraph_max_uid);

  if (VEC_length (ipa_node_params_t, ipa_node_params_vector)
      <= (unsigned) cgraph_max_uid)
    VEC_safe_grow_cleared (ipa_node_params_t, heap,
			   ipa_node_params_vector, cgraph_max_uid + 1);
}

/* This function ensures the array of edge arguments infos is big enough to
   accommodate a structure for all edges and reallocates it if not.  */

static inline void
ipa_check_create_edge_args (void)
{
  if (!ipa_edge_args_vector)
    ipa_edge_args_vector = VEC_alloc (ipa_edge_args_t, gc,
				      cgraph_edge_max_uid);

  if (VEC_length (ipa_edge_args_t, ipa_edge_args_vector)
      <=  (unsigned) cgraph_edge_max_uid)
    VEC_safe_grow_cleared (ipa_edge_args_t, gc, ipa_edge_args_vector,
			   cgraph_edge_max_uid + 1);
}

/* Returns true if the array of edge infos is large enough to accommodate an
   info for EDGE.  The main purpose of this function is that debug dumping
   function can check info availability without causing reallocations.  */

static inline bool
ipa_edge_args_info_available_for_edge_p (struct cgraph_edge *edge)
{
  return ((unsigned) edge->uid < VEC_length (ipa_edge_args_t,
					     ipa_edge_args_vector));
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
void ipa_push_func_to_list_1 (struct ipa_func_list **, struct cgraph_node *,
			      struct ipa_node_params *);
struct cgraph_node *ipa_pop_func_from_list (struct ipa_func_list **);

/* Add cgraph NODE to the worklist WL if it is not already in one.  */

static inline void
ipa_push_func_to_list (struct ipa_func_list **wl, struct cgraph_node *node)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);

  if (!info->node_enqueued)
    ipa_push_func_to_list_1 (wl, node, info);
}

void ipa_analyze_node (struct cgraph_node *);

/* Function formal parameters related computations.  */
void ipa_initialize_node_params (struct cgraph_node *node);
bool ipa_propagate_indirect_call_infos (struct cgraph_edge *cs,
					VEC (cgraph_edge_p, heap) **new_edges);

/* Indirect edge and binfo processing.  */
struct cgraph_edge *ipa_make_edge_direct_to_target (struct cgraph_edge *, tree,
						    tree);


/* Debugging interface.  */
void ipa_print_node_params (FILE *, struct cgraph_node *node);
void ipa_print_all_params (FILE *);
void ipa_print_node_jump_functions (FILE *f, struct cgraph_node *node);
void ipa_print_all_jump_functions (FILE * f);

/* Structure to describe transformations of formal parameters and actual
   arguments.  Each instance describes one new parameter and they are meant to
   be stored in a vector.  Additionally, most users will probably want to store
   adjustments about parameters that are being removed altogether so that SSA
   names belonging to them can be replaced by SSA names of an artificial
   variable.  */
struct ipa_parm_adjustment
{
  /* The original PARM_DECL itself, helpful for processing of the body of the
     function itself.  Intended for traversing function bodies.
     ipa_modify_formal_parameters, ipa_modify_call_arguments and
     ipa_combine_adjustments ignore this and use base_index.
     ipa_modify_formal_parameters actually sets this.  */
  tree base;

  /* Type of the new parameter.  However, if by_ref is true, the real type will
     be a pointer to this type.  */
  tree type;

  /* Alias refrerence type to be used in MEM_REFs when adjusting caller
     arguments.  */
  tree alias_ptr_type;

  /* The new declaration when creating/replacing a parameter.  Created by
     ipa_modify_formal_parameters, useful for functions modifying the body
     accordingly. */
  tree reduction;

  /* New declaration of a substitute variable that we may use to replace all
     non-default-def ssa names when a parm decl is going away.  */
  tree new_ssa_base;

  /* If non-NULL and the original parameter is to be removed (copy_param below
     is NULL), this is going to be its nonlocalized vars value.  */
  tree nonlocal_value;

  /* Offset into the original parameter (for the cases when the new parameter
     is a component of an original one).  */
  HOST_WIDE_INT offset;

  /* Zero based index of the original parameter this one is based on.  (ATM
     there is no way to insert a new parameter out of the blue because there is
     no need but if it arises the code can be easily exteded to do so.)  */
  int base_index;

  /* This new parameter is an unmodified parameter at index base_index. */
  unsigned copy_param : 1;

  /* This adjustment describes a parameter that is about to be removed
     completely.  Most users will probably need to book keep those so that they
     don't leave behinfd any non default def ssa names belonging to them.  */
  unsigned remove_param : 1;

  /* The parameter is to be passed by reference.  */
  unsigned by_ref : 1;
};

typedef struct ipa_parm_adjustment ipa_parm_adjustment_t;
DEF_VEC_O (ipa_parm_adjustment_t);
DEF_VEC_ALLOC_O (ipa_parm_adjustment_t, heap);

typedef VEC (ipa_parm_adjustment_t, heap) *ipa_parm_adjustment_vec;

VEC(tree, heap) *ipa_get_vector_of_formal_parms (tree fndecl);
void ipa_modify_formal_parameters (tree fndecl, ipa_parm_adjustment_vec,
				   const char *);
void ipa_modify_call_arguments (struct cgraph_edge *, gimple,
				ipa_parm_adjustment_vec);
ipa_parm_adjustment_vec ipa_combine_adjustments (ipa_parm_adjustment_vec,
						 ipa_parm_adjustment_vec);
void ipa_dump_param_adjustments (FILE *, ipa_parm_adjustment_vec, tree);

void ipa_prop_write_jump_functions (cgraph_node_set set);
void ipa_prop_read_jump_functions (void);
void ipa_update_after_lto_read (void);

/* From tree-sra.c:  */
tree build_ref_for_offset (location_t, tree, HOST_WIDE_INT, tree,
			   gimple_stmt_iterator *, bool);

#endif /* IPA_PROP_H */
