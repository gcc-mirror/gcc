/* { dg-do compile } */
/* { dg-options "-O0 -mrdrnd" } */

int
_rdrand16_step (unsigned short *__P)
{
  return __builtin_ia32_rdrand16_step (__P);
}
