/* { dg-do compile } */
/* { dg-options "-O2 -mfpmath=sse -msse2" } */
/* { dg-final { scan-assembler-times "comi" 1 } } */
/* { dg-final { scan-assembler-times "set" 1 } } */

int is_ordered_or_nonequal_sh (float a, float b)
{
  return !__builtin_isunordered (a, b) && (a != b);
}
