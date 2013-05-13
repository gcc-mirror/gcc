/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1 -fno-short-enums" } */

union tree_node;
typedef union tree_node *tree;
enum tree_code
{
  MAX_TREE_CODES
};
extern unsigned char tree_contains_struct[MAX_TREE_CODES][64];
struct tree_base
{
  __extension__ enum tree_code code:16;
  unsigned public_flag:1;
};
enum tree_node_structure_enum
{
  TS_DECL_WITH_VIS,
};
struct tree_decl_with_vis
{
  unsigned comdat_flag:1;
};
union tree_node
{
  struct tree_base base;
  struct tree_decl_with_vis decl_with_vis;
};
struct varpool_node
{
  tree decl;
  struct varpool_node *next_needed, *prev_needed;
  unsigned externally_visible:1;
};
extern struct varpool_node *varpool_nodes_queue;
struct pointer_set_t;
struct pointer_set_t *pointer_set_create (void);
__inline__ static unsigned char
varpool_externally_visible_p (struct varpool_node *vnode,
			      unsigned char aliased)
{
  struct varpool_node *alias;
  if (!(( { __typeof (vnode->decl) const __t = (vnode->decl); __t;})->decl_with_vis.comdat_flag)
      && !((vnode->decl)->base.public_flag))
    return 0;
  if (aliased)
    return 1;
  return 0;
}

unsigned int
function_and_variable_visibility (unsigned char whole_program)
{
  struct cgraph_node *node;
  struct varpool_node *vnode;
  struct pointer_set_t *aliased_vnodes = pointer_set_create ();
  for (vnode = varpool_nodes_queue; vnode; vnode = vnode->next_needed)
    if (varpool_externally_visible_p
	(vnode, pointer_set_contains (aliased_vnodes, vnode)))
      vnode->externally_visible = 1;
}

/* { dg-final { scan-tree-dump-not "& 255" "forwprop1"} } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */
