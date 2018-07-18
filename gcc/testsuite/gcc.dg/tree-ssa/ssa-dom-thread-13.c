/* { dg-do compile { target size32plus } } */ 
/* { dg-options "-O2 -fdump-tree-dom2-details -w" } */

union tree_node;
typedef union tree_node *tree;
extern unsigned char tree_contains_struct[0xdead][64];
struct tree_base
{
  int code:16;
};
struct tree_typed
{
  tree type;
};
struct tree_type_common
{
  tree main_variant;
};
extern tree build_target_option_node (void);
union tree_node
{
  struct tree_base base;
  struct tree_typed typed;
  struct tree_type_common type_common;
};
tree
convert (tree type, tree expr)
{
  tree e = expr;
  int code = (type)->base.code;
  const char *invalid_conv_diag;
  tree ret;
  if (tree_contains_struct[expr->base.code][(42)] != 1)
    abort ();
  if (type->type_common.main_variant == expr->typed.type->type_common.main_variant
      && (expr->typed.type->base.code != 123
	  || e->base.code == 456))
    return arf ();
  if (expr->typed.type->base.code == 42)
    error ("void value not ignored as it ought to be");
}

/* When the *->base.code tests in the second IF statement are false, we
   know that expr->typed.base->base.code has the value 123.  That allows
   us to thread the test for the final IF statement on that path.  */
/* { dg-final { scan-tree-dump-times "Threaded" 1 "dom2"} } */
