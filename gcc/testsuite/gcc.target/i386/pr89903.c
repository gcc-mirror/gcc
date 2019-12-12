/* PR target/89903 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -march=skylake" } */

int a, b;

void
foo (void)
{
  unsigned long long d = 983040;
  d += a;
  d >>= (short) d;
  b = d;
}
