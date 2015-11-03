/* { dg-do compile } */
/* { dg-require-effective-target arm_unaligned } */
/* { dg-options "-O2" } */

typedef unsigned int u32;
u32
read32 (const void* ptr)
{
  u32 v;
  __builtin_memcpy (&v, ptr, sizeof(v));
  return v;
}

/* { dg-final { scan-assembler "@ unaligned" } } */
