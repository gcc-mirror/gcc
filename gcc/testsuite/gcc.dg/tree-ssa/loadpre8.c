/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-pre-stats -std=gnu89 -fno-tree-loop-im" } */
typedef union tree_node *tree;
struct tree_common
{
  tree chain;
}
VEC_constructor_elt_base;
struct tree_ssa_name
{
  tree var;
};
union tree_node
{
  struct tree_common common;
  struct tree_ssa_name ssa_name;
};
struct edge_def
{
  struct basic_block_def *dest;
};
typedef struct edge_def *edge;
typedef struct VEC_edge_base
{
}
VEC_edge_base;
__attribute__ ((noinline)) static edge
VEC_edge_base_index (const VEC_edge_base * vec_, unsigned ix_)
{
}
typedef struct VEC_edge_gc
{
  VEC_edge_base base;
}
VEC_edge_gc;
struct basic_block_def
{
  VEC_edge_gc *succs;
};
typedef struct basic_block_def *basic_block;
typedef struct
{
  unsigned index;
  VEC_edge_gc **container;
}
edge_iterator;
__inline__ VEC_edge_gc *
ei_container (edge_iterator i)
{
  return *i.container;
}
__inline__ edge_iterator
ei_start_1 (VEC_edge_gc ** ev)
{
  edge_iterator i;
  i.container = ev;
  return i;
}
__attribute__ ((noinline)) static ei_next (edge_iterator * i)
{
}
static __inline__ edge
ei_edge (edge_iterator i)
{
  return  (edge) (VEC_edge_base_index ((((ei_container (i))) ? &((ei_container (i)))->base : 0), (i.index)));
}
static __inline__ unsigned char
ei_cond (edge_iterator ei, edge * p)
{
  *p = ei_edge (ei);
}
typedef tree *def_operand_p;
extern tree *get_phi_result_ptr (tree);
static __inline__ tree
get_def_from_ptr (def_operand_p def)
{
}
__attribute__ ((noinline)) static tree
phi_nodes (basic_block bb)
{
}

/* We can eliminate a load of the SRA'd variable edge_iterator.container */
rewrite_add_phi_arguments (basic_block bb)
{
  edge e;
  edge_iterator ei;
  for ((ei) = ei_start_1 (&((bb->succs))); ei_cond ((ei), &(e));
       ei_next (&(ei)))
    {
      tree phi;
      for (phi = phi_nodes (e->dest); phi; phi = (((phi))->common.chain))
	  get_reaching_def ((get_def_from_ptr (get_phi_result_ptr (phi)))->ssa_name.var);
    }
}
/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre" } } */
