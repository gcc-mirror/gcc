/* { dg-do compile } */
/* { dg-options "-msse -mfpmath=387" } */

float foo (float f)
{
  return __builtin_ia32_rsqrtf (f);
}
