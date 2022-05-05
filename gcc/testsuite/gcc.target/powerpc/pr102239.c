/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2" } */

void foo(long arg)
{
  if (arg & ((1UL << 33) | (1UL << 34)))
    asm volatile("# if");
  else
    asm volatile("# else");
}

/* { dg-final { scan-assembler-times {\mrldicr\.} 1 } } */
