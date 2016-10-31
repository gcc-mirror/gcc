/* PR tree-optimization/77860 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vrp -fno-tree-forwprop -Wno-psabi" } */

typedef unsigned short V __attribute__((vector_size (16)));

V
foo (V x, V y)
{
  V a = -x;
  V b = -y;
  return a * b;
}
