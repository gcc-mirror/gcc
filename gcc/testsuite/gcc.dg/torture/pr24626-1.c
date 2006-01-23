/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef long
(*bla)(int *node);

static long F2(void *tree, long blk, bla after_node_func)
{
 long call_result = 0;
 int *node;


 if (call_result = after_node_func(node))
  goto error_free_node;

 T(node);
 return 0;

error_free_node:
 T(node);
error:
 return call_result;
}

long F1(void *tree)
{
 return F2(tree, F3(tree), (void *)0);
}
