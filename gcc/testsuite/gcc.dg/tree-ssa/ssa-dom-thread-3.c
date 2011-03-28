/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-dom1-details" } */
extern void abort (void) __attribute__ ((__noreturn__));
union tree_node;
typedef union tree_node *tree;
enum tree_code
{
  VAR_DECL,
  SSA_NAME,
  MAX_TREE_CODES
};
extern unsigned char tree_contains_struct[MAX_TREE_CODES][64];
struct tree_base
{
  enum tree_code code:16;
};
enum tree_node_structure_enum
{
  TS_DECL_COMMON
};
struct tree_ssa_name
{
  tree var;
};
union tree_node
{
  struct tree_base base;
  struct tree_ssa_name ssa_name;
};
long
expand_one_var (tree var, unsigned char toplevel, unsigned char really_expand)
{
  tree origvar = var;
  var = var->ssa_name.var;
  if (((enum tree_code) (origvar)->base.code) == SSA_NAME
      && !((var->base.code != VAR_DECL)))
    abort ();
  if ((var->base.code) != VAR_DECL && ((origvar)->base.code) != SSA_NAME)
    ;
  else if (tree_contains_struct[(var->base.code)][(TS_DECL_COMMON)] != 1)
    abort ();
}
/* We should thread the jump, through an intermediate block.  */
/* { dg-final { scan-tree-dump-times "Threaded" 1 "dom1"} } */
/* { dg-final { scan-tree-dump-times "one or more intermediate" 1 "dom1"} } */
/* { dg-final { cleanup-tree-dump "dom1" } } */

