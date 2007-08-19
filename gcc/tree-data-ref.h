/* Data references and dependences detectors. 
   Copyright (C) 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <pop@cri.ensmp.fr>

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

#ifndef GCC_TREE_DATA_REF_H
#define GCC_TREE_DATA_REF_H

#include "graphds.h"
#include "lambda.h"
#include "omega.h"

/*
  innermost_loop_behavior describes the evolution of the address of the memory
  reference in the innermost enclosing loop.  The address is expressed as
  BASE + STEP * # of iteration, and base is further decomposed as the base
  pointer (BASE_ADDRESS),  loop invariant offset (OFFSET) and
  constant offset (INIT).  Examples, in loop nest 
  
  for (i = 0; i < 100; i++)
    for (j = 3; j < 100; j++)

                       Example 1                      Example 2
      data-ref         a[j].b[i][j]                   *(p + x + 16B + 4B * j)
      
  innermost_loop_behavior
      base_address     &a                             p
      offset           i * D_i			      x
      init             3 * D_j + offsetof (b)         28
      step             D_j                            4

  */
struct innermost_loop_behavior
{
  tree base_address;
  tree offset;
  tree init;
  tree step;

  /* Alignment information.  ALIGNED_TO is set to the largest power of two
     that divides OFFSET.  */
  tree aligned_to;
};

/* Describes the evolutions of indices of the memory reference.  The indices
   are indices of the ARRAY_REFs and the operands of INDIRECT_REFs.
   For ARRAY_REFs, BASE_OBJECT is the reference with zeroed indices
   (note that this reference does not have to be valid, if zero does not
   belong to the range of the array; hence it is not recommended to use
   BASE_OBJECT in any code generation).  For INDIRECT_REFs, the address is
   set to the loop-invariant part of the address of the object, except for
   the constant offset.  For the examples above,

   base_object:        a[0].b[0][0]                   *(p + x + 4B * j_0)
   indices:            {j_0, +, 1}_2                  {16, +, 4}_2
		       {i_0, +, 1}_1
		       {j_0, +, 1}_2
*/

struct indices
{
  /* The object.  */
  tree base_object;
  
  /* A list of chrecs.  Access functions of the indices.  */
  VEC(tree,heap) *access_fns;
};

struct dr_alias
{
  /* The alias information that should be used for new pointers to this
     location.  SYMBOL_TAG is either a DECL or a SYMBOL_MEMORY_TAG.  */
  tree symbol_tag;
  subvar_t subvars;
  struct ptr_info_def *ptr_info;

  /* The set of virtual operands corresponding to this memory reference,
     serving as a description of the alias information for the memory
     reference.  This could be eliminated if we had alias oracle.  */
  bitmap vops;
};

struct data_reference
{
  /* A pointer to the statement that contains this DR.  */
  tree stmt;
  
  /* A pointer to the memory reference.  */
  tree ref;

  /* Auxiliary info specific to a pass.  */
  void *aux;

  /* True when the data reference is in RHS of a stmt.  */
  bool is_read;

  /* Behavior of the memory reference in the innermost loop.  */
  struct innermost_loop_behavior innermost;

  /* Decomposition to indices for alias analysis.  */
  struct indices indices;

  /* Alias information for the data reference.  */
  struct dr_alias alias;
};

typedef struct data_reference *data_reference_p;
DEF_VEC_P(data_reference_p);
DEF_VEC_ALLOC_P (data_reference_p, heap);

#define DR_STMT(DR)                (DR)->stmt
#define DR_REF(DR)                 (DR)->ref
#define DR_BASE_OBJECT(DR)         (DR)->indices.base_object
#define DR_ACCESS_FNS(DR)	   (DR)->indices.access_fns
#define DR_ACCESS_FN(DR, I)        VEC_index (tree, DR_ACCESS_FNS (DR), I)
#define DR_NUM_DIMENSIONS(DR)      VEC_length (tree, DR_ACCESS_FNS (DR))  
#define DR_IS_READ(DR)             (DR)->is_read
#define DR_BASE_ADDRESS(DR)        (DR)->innermost.base_address
#define DR_OFFSET(DR)              (DR)->innermost.offset
#define DR_INIT(DR)                (DR)->innermost.init
#define DR_STEP(DR)                (DR)->innermost.step
#define DR_SYMBOL_TAG(DR)          (DR)->alias.symbol_tag
#define DR_PTR_INFO(DR)            (DR)->alias.ptr_info
#define DR_SUBVARS(DR)             (DR)->alias.subvars
#define DR_VOPS(DR)		   (DR)->alias.vops
#define DR_ALIGNED_TO(DR)          (DR)->innermost.aligned_to

enum data_dependence_direction {
  dir_positive, 
  dir_negative, 
  dir_equal, 
  dir_positive_or_negative,
  dir_positive_or_equal,
  dir_negative_or_equal,
  dir_star,
  dir_independent
};

/* The description of the grid of iterations that overlap.  At most
   two loops are considered at the same time just now, hence at most
   two functions are needed.  For each of the functions, we store
   the vector of coefficients, f[0] + x * f[1] + y * f[2] + ...,
   where x, y, ... are variables.  */

#define MAX_DIM 2

/* Special values of N.  */
#define NO_DEPENDENCE 0
#define NOT_KNOWN (MAX_DIM + 1)
#define CF_NONTRIVIAL_P(CF) ((CF)->n != NO_DEPENDENCE && (CF)->n != NOT_KNOWN)
#define CF_NOT_KNOWN_P(CF) ((CF)->n == NOT_KNOWN)
#define CF_NO_DEPENDENCE_P(CF) ((CF)->n == NO_DEPENDENCE)

typedef VEC (tree, heap) *affine_fn;

typedef struct
{
  unsigned n;
  affine_fn fns[MAX_DIM];
} conflict_function;

/* What is a subscript?  Given two array accesses a subscript is the
   tuple composed of the access functions for a given dimension.
   Example: Given A[f1][f2][f3] and B[g1][g2][g3], there are three
   subscripts: (f1, g1), (f2, g2), (f3, g3).  These three subscripts
   are stored in the data_dependence_relation structure under the form
   of an array of subscripts.  */

struct subscript
{
  /* A description of the iterations for which the elements are
     accessed twice.  */
  conflict_function *conflicting_iterations_in_a;
  conflict_function *conflicting_iterations_in_b;
  
  /* This field stores the information about the iteration domain
     validity of the dependence relation.  */
  tree last_conflict;
  
  /* Distance from the iteration that access a conflicting element in
     A to the iteration that access this same conflicting element in
     B.  The distance is a tree scalar expression, i.e. a constant or a
     symbolic expression, but certainly not a chrec function.  */
  tree distance;
};

typedef struct subscript *subscript_p;
DEF_VEC_P(subscript_p);
DEF_VEC_ALLOC_P (subscript_p, heap);

#define SUB_CONFLICTS_IN_A(SUB) SUB->conflicting_iterations_in_a
#define SUB_CONFLICTS_IN_B(SUB) SUB->conflicting_iterations_in_b
#define SUB_LAST_CONFLICT(SUB) SUB->last_conflict
#define SUB_DISTANCE(SUB) SUB->distance

/* A data_dependence_relation represents a relation between two
   data_references A and B.  */

struct data_dependence_relation
{
  
  struct data_reference *a;
  struct data_reference *b;

  /* When the dependence relation is affine, it can be represented by
     a distance vector.  */
  bool affine_p;

  /* A "yes/no/maybe" field for the dependence relation:
     
     - when "ARE_DEPENDENT == NULL_TREE", there exist a dependence
       relation between A and B, and the description of this relation
       is given in the SUBSCRIPTS array,
     
     - when "ARE_DEPENDENT == chrec_known", there is no dependence and
       SUBSCRIPTS is empty,
     
     - when "ARE_DEPENDENT == chrec_dont_know", there may be a dependence,
       but the analyzer cannot be more specific.  */
  tree are_dependent;
  
  /* For each subscript in the dependence test, there is an element in
     this array.  This is the attribute that labels the edge A->B of
     the data_dependence_relation.  */
  VEC (subscript_p, heap) *subscripts;

  /* The analyzed loop nest.  */
  VEC (loop_p, heap) *loop_nest;

  /* An index in loop_nest for the innermost loop that varies for
     this data dependence relation.  */
  unsigned inner_loop;

  /* The classic direction vector.  */
  VEC (lambda_vector, heap) *dir_vects;

  /* The classic distance vector.  */
  VEC (lambda_vector, heap) *dist_vects;

  /* Is the dependence reversed with respect to the lexicographic order?  */
  bool reversed_p;
};

typedef struct data_dependence_relation *ddr_p;
DEF_VEC_P(ddr_p);
DEF_VEC_ALLOC_P(ddr_p,heap);

#define DDR_A(DDR) DDR->a
#define DDR_B(DDR) DDR->b
#define DDR_AFFINE_P(DDR) DDR->affine_p
#define DDR_ARE_DEPENDENT(DDR) DDR->are_dependent
#define DDR_SUBSCRIPTS(DDR) DDR->subscripts
#define DDR_SUBSCRIPT(DDR, I) VEC_index (subscript_p, DDR_SUBSCRIPTS (DDR), I)
#define DDR_NUM_SUBSCRIPTS(DDR) VEC_length (subscript_p, DDR_SUBSCRIPTS (DDR))

#define DDR_LOOP_NEST(DDR) DDR->loop_nest
/* The size of the direction/distance vectors: the number of loops in
   the loop nest.  */
#define DDR_NB_LOOPS(DDR) (VEC_length (loop_p, DDR_LOOP_NEST (DDR)))
#define DDR_INNER_LOOP(DDR) DDR->inner_loop

#define DDR_DIST_VECTS(DDR) ((DDR)->dist_vects)
#define DDR_DIR_VECTS(DDR) ((DDR)->dir_vects)
#define DDR_NUM_DIST_VECTS(DDR) \
  (VEC_length (lambda_vector, DDR_DIST_VECTS (DDR)))
#define DDR_NUM_DIR_VECTS(DDR) \
  (VEC_length (lambda_vector, DDR_DIR_VECTS (DDR)))
#define DDR_DIR_VECT(DDR, I) \
  VEC_index (lambda_vector, DDR_DIR_VECTS (DDR), I)
#define DDR_DIST_VECT(DDR, I) \
  VEC_index (lambda_vector, DDR_DIST_VECTS (DDR), I)
#define DDR_REVERSED_P(DDR) DDR->reversed_p



/* Describes a location of a memory reference.  */

typedef struct data_ref_loc_d
{
  /* Position of the memory reference.  */
  tree *pos;

  /* True if the memory reference is read.  */
  bool is_read;
} data_ref_loc;

DEF_VEC_O (data_ref_loc);
DEF_VEC_ALLOC_O (data_ref_loc, heap);

bool get_references_in_stmt (tree, VEC (data_ref_loc, heap) **);
void dr_analyze_innermost (struct data_reference *);
extern void compute_data_dependences_for_loop (struct loop *, bool,
					       VEC (data_reference_p, heap) **,
					       VEC (ddr_p, heap) **);
extern void print_direction_vector (FILE *, lambda_vector, int);
extern void print_dir_vectors (FILE *, VEC (lambda_vector, heap) *, int);
extern void print_dist_vectors (FILE *, VEC (lambda_vector, heap) *, int);
extern void dump_subscript (FILE *, struct subscript *);
extern void dump_ddrs (FILE *, VEC (ddr_p, heap) *);
extern void dump_dist_dir_vectors (FILE *, VEC (ddr_p, heap) *);
extern void dump_data_reference (FILE *, struct data_reference *);
extern void dump_data_references (FILE *, VEC (data_reference_p, heap) *);
extern void debug_data_dependence_relation (struct data_dependence_relation *);
extern void dump_data_dependence_relation (FILE *, 
					   struct data_dependence_relation *);
extern void dump_data_dependence_relations (FILE *, VEC (ddr_p, heap) *);
extern void dump_data_dependence_direction (FILE *, 
					    enum data_dependence_direction);
extern void free_dependence_relation (struct data_dependence_relation *);
extern void free_dependence_relations (VEC (ddr_p, heap) *);
extern void free_data_refs (VEC (data_reference_p, heap) *);
struct data_reference *create_data_ref (struct loop *, tree, tree, bool);
bool find_loop_nest (struct loop *, VEC (loop_p, heap) **);
void compute_all_dependences (VEC (data_reference_p, heap) *,
			      VEC (ddr_p, heap) **, VEC (loop_p, heap) *, bool);



/* A RDG vertex representing a statement.  */
typedef struct rdg_vertex
{
  /* The statement represented by this vertex.  */
  tree stmt;
} *rdg_vertex_p;

#define RDGV_STMT(V)       ((struct rdg_vertex *) ((V)->data))->stmt

/* Data dependence type.  */

enum rdg_dep_type 
{
  /* Read After Write (RAW).  */
  flow_dd = 'f',
  
  /* Write After Read (WAR).  */
  anti_dd = 'a',
  
  /* Write After Write (WAW).  */
  output_dd = 'o', 
  
  /* Read After Read (RAR).  */
  input_dd = 'i' 
};

/* Dependence information attached to an edge of the RDG.  */

typedef struct rdg_edge 
{
  /* Type of the dependence.  */
  enum rdg_dep_type type;
} *rdg_edge_p;

#define RDGE_TYPE(E)        ((struct rdg_edge *) ((E)->data))->type

struct graph *build_rdg (struct loop *);

/* Return the index of the variable VAR in the LOOP_NEST array.  */

static inline int
index_in_loop_nest (int var, VEC (loop_p, heap) *loop_nest)
{
  struct loop *loopi;
  int var_index;

  for (var_index = 0; VEC_iterate (loop_p, loop_nest, var_index, loopi);
       var_index++)
    if (loopi->num == var)
      break;

  return var_index;
}

/* In lambda-code.c  */
bool lambda_transform_legal_p (lambda_trans_matrix, int, VEC (ddr_p, heap) *);

/* In tree-data-refs.c  */
void split_constant_offset (tree , tree *, tree *);

#endif  /* GCC_TREE_DATA_REF_H  */
