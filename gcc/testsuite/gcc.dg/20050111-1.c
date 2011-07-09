/* PR middle-end/19084, rtl-optimization/19348 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -march=i686" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

unsigned int
foo (unsigned long long x)
{
  unsigned int u;

  if (x == 0)
    return 0;
  u = (unsigned int) (x >> 32);
  return u;
}

unsigned long long
bar (unsigned short x)
{
  return (unsigned long long) x << 32;
}
