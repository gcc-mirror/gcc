/* PR middle-end/19084, rtl-optimization/19348 */
/* { dg-do compile } */
/* The following ensures that this test is compiled with -O2, unless
   on i?86 or x86_64 with -m32 option.  */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -march=i686" { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2" { target lp64 } } */

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
