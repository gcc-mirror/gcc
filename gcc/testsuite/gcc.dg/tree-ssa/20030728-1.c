/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
    

union tree_node;
typedef union tree_node *tree;

enum tree_code
{
  ARRAY_TYPE,
  LAST_AND_UNUSED_TREE_CODE
};

struct tree_common
{
  enum tree_code code:8;
};





union tree_node
{
  struct tree_common common;
};




int
objects_must_conflict_p (t1, t2)
     tree t1, t2;
{

  if ((t1->common.code == ARRAY_TYPE) != (t2
                                          && t2->common.code == ARRAY_TYPE))
    return 11;


  return foo (t2 ? get_alias_set (t2) : 0);
}

/* There should be two assignments of variables to the value zero.  */
/* { dg-final { scan-tree-dump-times " = 0" 2 "optimized"} } */
 
/* { dg-final { cleanup-tree-dump "optimized" } } */
