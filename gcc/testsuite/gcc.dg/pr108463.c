/* PR rtl-optimization/108463 */
/* { dg-do compile } */
/* { dg-options "-O2 -fsched2-use-superblocks -fcompare-debug -Wno-psabi" } */

typedef int __attribute__((__vector_size__ (32))) V;
int a;

void
foo (V v)
{
  a--;
  v = (V) v;
}
