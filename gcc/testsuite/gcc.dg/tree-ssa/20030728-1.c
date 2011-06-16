/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-expand-details" } */
    

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

/* There should be one assignment of variables to the value zero.  There
   used to be two assignments, but improvements in threading allowed the
   second to be propagated into all its uses and eliminated.   */
/* { dg-final { scan-rtl-dump-times "PART.. = 0" 1 "expand"} } */
 
/* { dg-final { cleanup-rtl-dump "expand" } } */
