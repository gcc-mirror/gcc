/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom3" } */
  
extern void abort (void);

union tree_node;
typedef union tree_node *tree;
enum tree_code
{
  TREE_VEC = 20,
};
struct tree_common
{
  int code;
};
struct tree_type
{
  tree binfo;
};
union tree_node
{
  struct tree_common common;
  struct tree_type type;
};
void
record_component_aliases (type)
     tree type;
{
  const tree __z = type->type.binfo;
  if (type->type.binfo->common.code != TREE_VEC)
    abort ();

  if (__z->common.code != TREE_VEC)
    abort ();
}

/* There should be precisely one load of type.binfo.  If there is
   more than one, then the dominator optimizations failed.  */
/* { dg-final { scan-tree-dump-times "type\\.binfo" 1 "dom3"} } */
 
/* There should be precisely one load of common.code.  If there is
   more than one, then the dominator optimizations failed.  */
/* { dg-final { scan-tree-dump-times "common\\.code" 1 "dom3"} } */
 
/* There should be one IF conditional.  */
/* { dg-final { scan-tree-dump-times "if " 1 "dom3"} } */
