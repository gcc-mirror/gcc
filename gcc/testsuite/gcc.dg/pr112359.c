/* { dg-do compile { target aarch64*-*-* i?86-*-* x86_64-*-* } } */
/* { dg-additional-options "-O -std=gnu99 -mavx512fp16 -ftree-loop-if-convert" { target i?86-*-* x86_64-*-* } } */
/* { dg-additional-options "-O -std=gnu99 -march=armv8.4-a+sve -ftree-loop-if-convert" { target aarch64*-*-* } } */

int i, c;
unsigned long long u;

void
foo (void)
{
  for (; i; i++)
    if (c)
      u |= i;
}

