/* { dg-do compile } */
/* { dg-options "-O1 -mabi=windowed" } */

void test(int *p)
{
  register int sp asm("sp") = 0;
  asm volatile ("# %0"::""(sp));
  sp = *p;
  asm volatile ("# %0"::""(sp));
}

/* { dg-final { scan-assembler-times "movsp" 2 } } */
