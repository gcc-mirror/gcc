/* { dg-do compile } */
/* { dg-options "-fno-tree-sra" } */

_Complex float foo(void)
{
  _Complex float a[64] = {};
  _Complex float x;
  x =  a[1];
  return x;
}

