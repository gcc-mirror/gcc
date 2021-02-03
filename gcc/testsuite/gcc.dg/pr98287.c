/* PR tree-optimization/98287 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ccp -fno-tree-forwprop -Wno-psabi -w" } */

typedef unsigned long __attribute__((__vector_size__ (8))) V;
V v;

static __attribute__((noinline, noclone)) V
bar (unsigned short s)
{
  return v >> s << s | v >> s >> 63;
}

unsigned long
foo (void)
{
  V x = bar (1);
  return x[0];
}
