/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O3" } */
/* { dg-add-options arm_neon } */


int
t6 (int len, void * dummy, int * __restrict x)
{
  len = len & ~31;
  long long result = 0;
  __asm volatile ("");
  for (int i = 0; i < len; i++)
    result += x[i];
  return result;
}

/* { dg-final { scan-assembler "vaddw\.s32" } } */
