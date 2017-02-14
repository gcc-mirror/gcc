/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Test that conversion from 32-bit and 64-bit integers can be done
   without a call to the support library.  */

#pragma GCC target ("arch=armv8.2-a+nofp16")

__fp16
foo (int x)
{
  return x;
}

__fp16
bar (unsigned int x)
{
  return x;
}

__fp16
fool (long long x)
{
  return x;
}

__fp16
barl (unsigned long long x)
{
  return x;
}


/* { dg-final { scan-assembler-not "__float\\\[ds\\\]ihf2" } } */
/* { dg-final { scan-assembler-not "__floatun\\\[ds\\\]ihf2" } } */
