/* Data references and dependences detectors.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
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
#include "omega.h"
#include "tree-chrec.h"

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
  struct ptr_info_def *ptr_info;

  /* The set of virtual operands corresponding to this memory reference,
     serving as a description of the alias information for the memory
     reference.  This could be eliminated if we had alias oracle.  */
  bitmap vops;
};

/* An integer vector.  A vector formally consists of an element of a vector
   space. A vector space is a set that is closed under vector addition
   and scalar multiplication.  In this vector space, an element is a list of
   integers.  */
typedef int *lambda_vector;
DEF_VEC_P(lambda_vector);
DEF_VEC_ALLOC_P(lambda_vector,heap);
DEF_VEC_ALLOC_P(lambda_vector,gc);

/* An integer matrix.  A matrix consists of m vectors of length n (IE
   all vectors are the same length).  */
typedef lambda_vector *lambda_matrix;

/* Each vector of the access matrix represents a linear access
   function for a subscript.  First elements correspond to the
   leftmost indices, ie. for a[i][j] the first vector corresponds to
   the subscript in "i".  The elements of a vector are relative to
   the loop nests in which the data reference is considered,
   i.e. the vector is relative to the SCoP that provides the context
   in which this data reference occurs.

   For example, in

   | loop_1
   |    loop_2
   |      a[i+3][2*j+n-1]

   if "i" varies in loop_1 and "j" varies in loop_2, the access
   matrix with respect to the loop nest {loop_1, loop_2} is:

   | loop_1  loop_2  param_n  cst
   |   1       0        0      3
   |   0       2        1     -1

   whereas the access matrix with respect to loop_2 considers "i" as
   a parameter:

   | loop_2  param_i  param_n  cst
   |   0       1         0      3
   |   2       0         1     -1
*/
struct access_matrix
{
  VEC (loop_p, heap) *loop_nest;
  int nb_induction_vars;
  VEC (tree, heap) *parameters;
  VEC (lambda_vector, gc) *matrix;
};

#define AM_LOOP_NEST(M) (M)->loop_nest
#define AM_NB_INDUCTION_VARS(M) (M)->nb_induction_vars
#define AM_PARAMETERS(M) (M)->parameters
#define AM_MATRIX(M) (M)->matrix
#define AM_NB_PARAMETERS(M) (VEC_length (tree, AM_PARAMETERS(M)))
#define AM_CONST_COLUMN_INDEX(M) (AM_NB_INDUCTION_VARS (M) + AM_NB_PARAMETERS (M))
#define AM_NB_COLUMNS(M) (AM_NB_INDUCTION_VARS (M) + AM_NB_PARAMETERS (M) + 1)
#define AM_GET_SUBSCRIPT_ACCESS_VECTOR(M, I) VEC_index (lambda_vector, AM_MATRIX (M), I)
#define AM_GET_ACCESS_MATRIX_ELEMENT(M, I, J) AM_GET_SUBSCRIPT_ACCESS_VECTOR (M, I)[J]

/* Return the column in the access matrix of LOOP_NUM.  */

static inline int
am_vector_index_for_loop (struct access_matrix *access_matrix, int loop_num)
{
  int i;
  loop_p l;

  for (i = 0; VEC_iterate (loop_p, AM_LOOP_NEST (access_matrix), i, l); i++)
    if (l->num == loop_num)
      return i;

  gcc_unreachable();
}

int access_matrix_get_index_for_parameter (tree, struct access_matrix *);

struct data_reference
{
  /* A pointer to the statement that contains this DR.  */
  gimple stmt;

  /* A pointer to the memory reference.  */
  tree ref;

  /* Auxiliary info specific to a pass.  */
  void *aux;

  /* True when the data reference is in RHS of a stmt.  */
  bool is_read;

  /* Behavior of the memory reference in the innermost loop.  */
  struct innermost_loop_behavior innermost;

  /* Subscripts of this data reference.  */
  struct indices indices;

  /* Alias information for the data reference.  */
  struct dr_alias alias;

  /* Matrix representation for the data access functions.  */
  struct access_matrix *access_matrix;
};

#define DR_STMT(DR)                (DR)->stmt
#define DR_REF(DR)                 (DR)->ref
#define DR_BASE_OBJECT(DR)         (DR)->indices.base_object
#define DR_ACCESS_FNS(DR)	   (DR)->indices.access_fns
#define DR_ACCESS_FN(DR, I)        VEC_index (tree, DR_ACCESS_FNS (DR), I)
#define DR_NUM_DIMENSIONS(DR)      VEC_length (tree, DR_ACCESS_FNS (DR))
#define DR_IS_READ(DR)             (DR)->is_read
#define DR_IS_WRITE(DR)            (!DR_IS_READ (DR))
#define DR_BASE_ADDRESS(DR)        (DR)->innermost.base_address
#define DR_OFFSET(DR)              (DR)->innermost.offset
#define DR_INIT(DR)                (DR)->innermost.init
#define DR_STEP(DR)                (DR)->innermost.step
#define DR_PTR_INFO(DR)            (DR)->alias.ptr_info
#define DR_ALIGNED_TO(DR)          (DR)->innermost.aligned_to
#define DR_ACCESS_MATRIX(DR)       (DR)->access_matrix

typedef struct data_reference *data_reference_p;
DEF_VEC_P(data_reference_p);
DEF_VEC_ALLOC_P (data_reference_p, heap);

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

  /* The classic direction vector.  */
  VEC (lambda_vector, heap) *dir_vects;

  /* The classic distance vector.  */
  VEC (lambda_vector, heap) *dist_vects;

  /* An index in loop_nest for the innermost loop that varies for
     this data dependence relation.  */
  unsigned inner_loop;

  /* Is the dependence reversed with respect to the lexicographic order?  */
  bool reversed_p;

  /* When the dependence relation is affine, it can be represented by
     a distance vector.  */
  bool affine_p;

  /* Set to true when the dependence relation is on the same data
     access.  */
  bool self_reference_p;
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
#define DDR_SELF_REFERENCE(DDR) DDR->self_reference_p

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

bool get_references_in_stmt (gimple, VEC (data_ref_loc, heap) **);
bool dr_analyze_innermost (struct data_reference *);
extern bool compute_data_dependences_for_loop (struct loop *, bool,
					       VEC (loop_p, heap) **,
					       VEC (data_reference_p, heap) **,
					       VEC (ddr_p, heap) **);
extern bool compute_data_dependences_for_bb (basic_block, bool,
                                             VEC (data_reference_p, heap) **,
                                             VEC (ddr_p, heap) **);
extern tree find_data_references_in_loop (struct loop *,
                                          VEC (data_reference_p, heap) **);
extern void print_direction_vector (FILE *, lambda_vector, int);
extern void print_dir_vectors (FILE *, VEC (lambda_vector, heap) *, int);
extern void print_dist_vectors (FILE *, VEC (lambda_vector, heap) *, int);
extern void dump_subscript (FILE *, struct subscript *);
extern void dump_ddrs (FILE *, VEC (ddr_p, heap) *);
extern void dump_dist_dir_vectors (FILE *, VEC (ddr_p, heap) *);
extern void dump_data_reference (FILE *, struct data_reference *);
extern void debug_data_reference (struct data_reference *);
extern void dump_data_references (FILE *, VEC (data_reference_p, heap) *);
extern void debug_data_references (VEC (data_reference_p, heap) *);
extern void debug_data_dependence_relation (struct data_dependence_relation *);
extern void dump_data_dependence_relation (FILE *,
					   struct data_dependence_relation *);
extern void dump_data_dependence_relations (FILE *, VEC (ddr_p, heap) *);
extern void debug_data_dependence_relations (VEC (ddr_p, heap) *);
extern void dump_data_dependence_direction (FILE *,
					    enum data_dependence_direction);
extern void free_dependence_relation (struct data_dependence_relation *);
extern void free_dependence_relations (VEC (ddr_p, heap) *);
extern void free_data_ref (data_reference_p);
extern void free_data_refs (VEC (data_reference_p, heap) *);
extern bool find_data_references_in_stmt (struct loop *, gimple,
					  VEC (data_reference_p, heap) **);
extern bool graphite_find_data_references_in_stmt (loop_p, loop_p, gimple,
						   VEC (data_reference_p, heap) **);
struct data_reference *create_data_ref (loop_p, loop_p, tree, gimple, bool);
extern bool find_loop_nest (struct loop *, VEC (loop_p, heap) **);
extern void compute_all_dependences (VEC (data_reference_p, heap) *,
				     VEC (ddr_p, heap) **, VEC (loop_p, heap) *,
				     bool);
extern tree find_data_references_in_bb (struct loop *, basic_block,
                                        VEC (data_reference_p, heap) **);

extern void create_rdg_vertices (struct graph *, VEC (gimple, heap) *);
extern bool dr_may_alias_p (const struct data_reference *,
			    const struct data_reference *);
extern bool dr_equal_offsets_p (struct data_reference *,
                                struct data_reference *);


/* Return true when the base objects of data references A and B are
   the same memory object.  */

static inline bool
same_data_refs_base_objects (data_reference_p a, data_reference_p b)
{
  return DR_NUM_DIMENSIONS (a) == DR_NUM_DIMENSIONS (b)
    && operand_equal_p (DR_BASE_OBJECT (a), DR_BASE_OBJECT (b), 0);
}

/* Return true when the data references A and B are accessing the same
   memory object with the same access functions.  */

static inline bool
same_data_refs (data_reference_p a, data_reference_p b)
{
  unsigned int i;

  /* The references are exactly the same.  */
  if (operand_equal_p (DR_REF (a), DR_REF (b), 0))
    return true;

  if (!same_data_refs_base_objects (a, b))
    return false;

  for (i = 0; i < DR_NUM_DIMENSIONS (a); i++)
    if (!eq_evolutions_p (DR_ACCESS_FN (a, i), DR_ACCESS_FN (b, i)))
      return false;

  return true;
}

/* Return true when the DDR contains two data references that have the
   same access functions.  */

static inline bool
same_access_functions (const struct data_dependence_relation *ddr)
{
  unsigned i;

  for (i = 0; i < DDR_NUM_SUBSCRIPTS (ddr); i++)
    if (!eq_evolutions_p (DR_ACCESS_FN (DDR_A (ddr), i),
			  DR_ACCESS_FN (DDR_B (ddr), i)))
      return false;

  return true;
}

/* Return true when DDR is an anti-dependence relation.  */

static inline bool
ddr_is_anti_dependent (ddr_p ddr)
{
  return (DDR_ARE_DEPENDENT (ddr) == NULL_TREE
	  && DR_IS_READ (DDR_A (ddr))
	  && DR_IS_WRITE (DDR_B (ddr))
	  && !same_access_functions (ddr));
}

/* Return true when DEPENDENCE_RELATIONS contains an anti-dependence.  */

static inline bool
ddrs_have_anti_deps (VEC (ddr_p, heap) *dependence_relations)
{
  unsigned i;
  ddr_p ddr;

  for (i = 0; VEC_iterate (ddr_p, dependence_relations, i, ddr); i++)
    if (ddr_is_anti_dependent (ddr))
      return true;

  return false;
}

/* Returns the dependence level for a vector DIST of size LENGTH.
   LEVEL = 0 means a lexicographic dependence, i.e. a dependence due
   to the sequence of statements, not carried by any loop.  */

static inline unsigned
dependence_level (lambda_vector dist_vect, int length)
{
  int i;

  for (i = 0; i < length; i++)
    if (dist_vect[i] != 0)
      return i + 1;

  return 0;
}

/* Return the dependence level for the DDR relation.  */

static inline unsigned
ddr_dependence_level (ddr_p ddr)
{
  unsigned vector;
  unsigned level = 0;

  if (DDR_DIST_VECTS (ddr))
    level = dependence_level (DDR_DIST_VECT (ddr, 0), DDR_NB_LOOPS (ddr));

  for (vector = 1; vector < DDR_NUM_DIST_VECTS (ddr); vector++)
    level = MIN (level, dependence_level (DDR_DIST_VECT (ddr, vector),
					  DDR_NB_LOOPS (ddr)));
  return level;
}



/* A Reduced Dependence Graph (RDG) vertex representing a statement.  */
typedef struct rdg_vertex
{
  /* The statement represented by this vertex.  */
  gimple stmt;

  /* True when the statement contains a write to memory.  */
  bool has_mem_write;

  /* True when the statement contains a read from memory.  */
  bool has_mem_reads;
} *rdg_vertex_p;

#define RDGV_STMT(V)     ((struct rdg_vertex *) ((V)->data))->stmt
#define RDGV_HAS_MEM_WRITE(V) ((struct rdg_vertex *) ((V)->data))->has_mem_write
#define RDGV_HAS_MEM_READS(V) ((struct rdg_vertex *) ((V)->data))->has_mem_reads
#define RDG_STMT(RDG, I) RDGV_STMT (&(RDG->vertices[I]))
#define RDG_MEM_WRITE_STMT(RDG, I) RDGV_HAS_MEM_WRITE (&(RDG->vertices[I]))
#define RDG_MEM_READS_STMT(RDG, I) RDGV_HAS_MEM_READS (&(RDG->vertices[I]))

void dump_rdg_vertex (FILE *, struct graph *, int);
void debug_rdg_vertex (struct graph *, int);
void dump_rdg_component (FILE *, struct graph *, int, bitmap);
void debug_rdg_component (struct graph *, int);
void dump_rdg (FILE *, struct graph *);
void debug_rdg (struct graph *);
int rdg_vertex_for_stmt (struct graph *, gimple);

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

  /* Levels of the dependence: the depth of the loops that carry the
     dependence.  */
  unsigned level;

  /* Dependence relation between data dependences, NULL when one of
     the vertices is a scalar.  */
  ddr_p relation;
} *rdg_edge_p;

#define RDGE_TYPE(E)        ((struct rdg_edge *) ((E)->data))->type
#define RDGE_LEVEL(E)       ((struct rdg_edge *) ((E)->data))->level
#define RDGE_RELATION(E)    ((struct rdg_edge *) ((E)->data))->relation

struct graph *build_rdg (struct loop *,
			 VEC (loop_p, heap) **,
			 VEC (ddr_p, heap) **,
			 VEC (data_reference_p, heap) **);
struct graph *build_empty_rdg (int);
void free_rdg (struct graph *);

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

void stores_from_loop (struct loop *, VEC (gimple, heap) **);
void stores_zero_from_loop (struct loop *, VEC (gimple, heap) **);
void remove_similar_memory_refs (VEC (gimple, heap) **);
bool rdg_defs_used_in_other_loops_p (struct graph *, int);
bool have_similar_memory_accesses (gimple, gimple);
bool stmt_with_adjacent_zero_store_dr_p (gimple);

/* Returns true when STRIDE is equal in absolute value to the size of
   the unit type of TYPE.  */

static inline bool
stride_of_unit_type_p (tree stride, tree type)
{
  return tree_int_cst_equal (fold_unary (ABS_EXPR, TREE_TYPE (stride),
					 stride),
			     TYPE_SIZE_UNIT (type));
}

/* Determines whether RDG vertices V1 and V2 access to similar memory
   locations, in which case they have to be in the same partition.  */

static inline bool
rdg_has_similar_memory_accesses (struct graph *rdg, int v1, int v2)
{
  return have_similar_memory_accesses (RDG_STMT (rdg, v1),
				       RDG_STMT (rdg, v2));
}

/* In tree-data-ref.c  */
void split_constant_offset (tree , tree *, tree *);

/* Strongly connected components of the reduced data dependence graph.  */

typedef struct rdg_component
{
  int num;
  VEC (int, heap) *vertices;
} *rdgc;

DEF_VEC_P (rdgc);
DEF_VEC_ALLOC_P (rdgc, heap);

DEF_VEC_P (bitmap);
DEF_VEC_ALLOC_P (bitmap, heap);

/* Compute the greatest common divisor of a VECTOR of SIZE numbers.  */

static inline int
lambda_vector_gcd (lambda_vector vector, int size)
{
  int i;
  int gcd1 = 0;

  if (size > 0)
    {
      gcd1 = vector[0];
      for (i = 1; i < size; i++)
	gcd1 = gcd (gcd1, vector[i]);
    }
  return gcd1;
}

/* Allocate a new vector of given SIZE.  */

static inline lambda_vector
lambda_vector_new (int size)
{
  return (lambda_vector) ggc_alloc_cleared_atomic (sizeof (int) * size);
}

/* Clear out vector VEC1 of length SIZE.  */

static inline void
lambda_vector_clear (lambda_vector vec1, int size)
{
  memset (vec1, 0, size * sizeof (*vec1));
}

/* Returns true when the vector V is lexicographically positive, in
   other words, when the first nonzero element is positive.  */

static inline bool
lambda_vector_lexico_pos (lambda_vector v,
			  unsigned n)
{
  unsigned i;
  for (i = 0; i < n; i++)
    {
      if (v[i] == 0)
	continue;
      if (v[i] < 0)
	return false;
      if (v[i] > 0)
	return true;
    }
  return true;
}

/* Return true if vector VEC1 of length SIZE is the zero vector.  */

static inline bool
lambda_vector_zerop (lambda_vector vec1, int size)
{
  int i;
  for (i = 0; i < size; i++)
    if (vec1[i] != 0)
      return false;
  return true;
}

/* Allocate a matrix of M rows x  N cols.  */

static inline lambda_matrix
lambda_matrix_new (int m, int n, struct obstack *lambda_obstack)
{
  lambda_matrix mat;
  int i;

  mat = (lambda_matrix) obstack_alloc (lambda_obstack,
				       sizeof (lambda_vector *) * m);

  for (i = 0; i < m; i++)
    mat[i] = lambda_vector_new (n);

  return mat;
}

#endif  /* GCC_TREE_DATA_REF_H  */
