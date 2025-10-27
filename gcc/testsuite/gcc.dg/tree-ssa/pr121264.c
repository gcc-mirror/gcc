/* PR tree-optimization/121264 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump " \\\| " "optimized" } } */

struct A { char b; char c[0x20000010]; } a;

int
foo ()
{
  return a.c[0x20000000] || a.c[1];
}
