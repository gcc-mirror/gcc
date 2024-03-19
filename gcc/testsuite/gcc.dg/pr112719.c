/* PR tree-optimization/112719 */
/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-additional-options "-msse4" { target i?86-*-* x86_64-*-* } } */

int
foo (unsigned int a, unsigned short b)
{
  return __builtin_popcountl (a) + __builtin_popcountl (b);
}

int
bar (unsigned int a, unsigned short b)
{
  a &= 0xaaaaaaaaUL;
  b &= 0x5555;
  return __builtin_popcountll (a) + __builtin_popcountll (b);
}
