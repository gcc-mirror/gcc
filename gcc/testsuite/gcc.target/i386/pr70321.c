/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2" } */

void foo (long long ixi)
{
  if (ixi != 14348907)
    __builtin_abort ();
}

/* { dg-final { scan-assembler-times "mov" 1 } } */
