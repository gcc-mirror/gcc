/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-optimized" } */

int a, b;

int __GIMPLE (ssa,guessed_local(1073741824))
main (int argc)
{
  int _1;
  int _2;
  int _4;

  __BB(2,guessed_local(1073741824)):
  _1 = a;
  _2 = b;
  _4 = __MAX (_1, _2);
  return _4;

}

/* { dg-final { scan-tree-dump "MAX_EXPR" "optimized" } } */
