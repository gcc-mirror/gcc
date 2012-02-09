/* PR target/52006 */
/* { dg-do compile } */
/* { dg-options "-march=armv7-a -mfloat-abi=hard -O2 -fPIC" } */

unsigned long a;
static int b;

void
foo (void)
{
  asm volatile ("" : "=r" (b));
}

void
bar (float f)
{
  if (f < b / 100.0)
    a = 1;
}
