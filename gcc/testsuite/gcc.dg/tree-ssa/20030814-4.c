/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom3 -fdump-tree-optimized" } */
    
extern void abort (void);
union tree_node;
typedef union tree_node *tree;
extern const char tree_code_type[];
struct tree_common
{
  int code;
};
struct tree_decl 
{
  long pointer_alias_set;
};
union tree_node
{
  struct tree_common common;
  struct tree_decl decl;
};
long
blah (decl, set)
     tree decl;
     long set;
{
  decl->decl.pointer_alias_set = set;
  if (tree_code_type[decl->common.code] != 'd')
    abort ();
  record_alias_subset (decl->decl.pointer_alias_set);
  if (set != -1)
    set = 0; 
  return set;
}

/* There should be precisely one reference to pointer_alias_set.  If there is
   more than one, then the dominator optimizations failed.  */
/* { dg-final { scan-tree-dump-times "pointer_alias_set" 1 "dom3"} } */

/* The assignment set = -1 in the ELSE clause of the last IF
   statement should be removed by the final cleanup phase.  */
/* { dg-final { scan-tree-dump-times "set = -1" 0 "optimized"} } */
