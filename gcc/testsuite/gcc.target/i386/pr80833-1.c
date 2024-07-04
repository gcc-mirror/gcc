/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1 -mtune=intel -mregparm=2 -mno-stackrealign" } */
/* { dg-require-effective-target ia32 } */

long long test (long long a)
{
  asm ("" : "+x" (a));
  return a;
}

/* { dg-final { scan-assembler "pinsrd" } } */
/* { dg-final { scan-assembler "pextrd" } } */
