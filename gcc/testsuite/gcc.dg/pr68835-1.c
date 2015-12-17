/* PR tree-optimization/68835 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

unsigned __int128
foo (unsigned long a, unsigned long b)
{
  unsigned __int128 x = (unsigned __int128) a * b;
  struct { unsigned __int128 a : 96; } w;
  w.a = x;
  return w.a;
}
