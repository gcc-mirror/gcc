/* PR rtl-optimization/114211 */
/* { dg-do run { target int128 } } */
/* { dg-options "-O -fno-tree-coalesce-vars -Wno-psabi" } */

typedef unsigned __int128 V __attribute__((__vector_size__ (16)));
unsigned int u;
V v;

V
foo (unsigned __int128 h)
{
  h = h << 64 | h >> 64;
  h *= ~u;
  return h + v;
}

int
main ()
{
  V x = foo (1);
  if (x[0] != (unsigned __int128) 0xffffffff << 64)
    __builtin_abort ();
}
