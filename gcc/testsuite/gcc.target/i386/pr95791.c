/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512f -mvzeroupper" } */

void
f(void)
{
  __asm__ __volatile__("" ::: "zmm16");
}

/* { dg-final { scan-assembler-not "vzeroupper" } } */
