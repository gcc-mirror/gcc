/* PR tree-optimization/120074 */
/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-copy-prop -fno-tree-forwprop -fno-tree-ccp" } */

int foo (int);
short a;
int b;

int
bar (int d, int e)
{
  return d < 0 || d > __INT_MAX__ >> e;
}

int
main ()
{
  int f = bar ((b ^ a) & 3, __SIZEOF_INT__ * __CHAR_BIT__ - 2);
  foo (f);
}
