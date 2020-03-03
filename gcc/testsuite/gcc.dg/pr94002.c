/* PR rtl-optimization/94002 */
/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-dce -fno-tree-reassoc" } */
/* { dg-additional-options "-fPIC" { target fpic } } */

unsigned a, b;

void
foo (void)
{
  __builtin_sub_overflow (b, 44852956282LL, &a);
  a += ~b;
}
