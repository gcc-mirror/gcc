/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom2" } */
  
extern void abort (void);

union tree_node;
typedef union tree_node *tree;
enum tree_code
{
  BIND_EXPR,
};
struct tree_common
{
  enum tree_code code:8;
};
union tree_node
{
  struct tree_common common;
};
tree
voidify_wrapper_expr (tree wrapper)
{
  switch (wrapper->common.code)
    {
    case BIND_EXPR:
      if (wrapper->common.code != BIND_EXPR)
        abort ();
    }
}


/* There should be no IF conditionals.  */
/* { dg-final { scan-tree-dump-times "if " 0 "dom2"} } */
