/* { dg-do compile } */
/* { dg-options "-mavx512f -O2 -mtune=generic" } */
/* { dg-final { scan-assembler-times {(?n)vpcmpeq.*zmm} 2 } } */
/* { dg-final { scan-assembler-times {(?n)kortest.*k[0-7]} 2 } } */

int compare (const char* s1, const char* s2)
{
  return __builtin_memcmp (s1, s2, 64) == 0;
}

int compare1 (const char* s1, const char* s2)
{
  return __builtin_memcmp (s1, s2, 64) != 0;
}
