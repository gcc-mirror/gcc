/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom2" } */
   

union tree_node;
typedef union tree_node *tree;
extern const char tree_code_type[];
struct tree_common
{
  int code;
  tree type;
};
struct tree_exp
{
  tree operands[1];
};
union tree_node
{
  struct tree_common common;
  struct tree_exp exp;
};
long
get_alias_set (t)
     tree t;
{
  if (tree_code_type[t->common.code] != 't' && t->common.type == 0)
    return 0;
  if (tree_code_type[t->common.code] != 't')
    {
      while (t->exp.operands[0])
        t = t->exp.operands[0];
    }
}

/* There should be exactly 4 IF conditionals if we thread jumps
   properly.  There used to be 3, but one thread was crossing
   loops.  */
/* { dg-final { scan-tree-dump-times "if " 4 "dom2"} } */
 
