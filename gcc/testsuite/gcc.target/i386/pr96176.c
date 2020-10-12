/* PR target/96176 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tmovzbl\t" } } */

unsigned char v;

void
foo (unsigned char *x, unsigned char y, unsigned char z)
{
  __atomic_compare_exchange_n (x, &y, z, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
  v = y;
}
