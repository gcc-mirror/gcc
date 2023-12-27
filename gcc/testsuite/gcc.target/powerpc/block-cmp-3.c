/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler-times {\mb[l]? memcmp\M} 1 } }  */

int foo (const char* s1, const char* s2)
{
  return __builtin_memcmp (s1, s2, 4);
}
