/* { dg-do compile } */
/* { dg-options "-Ofast -mdejagnu-cpu=power7" } */

#include <math.h>

float test1 (float x, float y)
{
  return fmodf (x, y);
}

double test2 (double x, double y)
{
  return fmod (x, y);
}

float test3 (float x, float y)
{
  return remainderf (x, y);
}

double test4 (double x, double y)
{
  return remainder (x, y);
}

/* { dg-final { scan-assembler-not {(?n)\mb.*fmod} } } */
/* { dg-final { scan-assembler-not {(?n)\mb.*fmodf} } } */
/* { dg-final { scan-assembler-not {(?n)\mb.*remainder} } } */
/* { dg-final { scan-assembler-not {(?n)\mb.*remainderf} } } */
/* { dg-final { scan-assembler-times {\mfdiv\M} 2 } } */
/* { dg-final { scan-assembler-times {\mfdivs\M} 2 } } */
/* { dg-final { scan-assembler-times {\mfnmsub\M} 2 } } */
/* { dg-final { scan-assembler-times {\mfnmsubs\M} 2 } } */
/* { dg-final { scan-assembler-times {\mfriz\M} 2 } } */
/* { dg-final { scan-assembler-times {\mfrin\M} 2 } } */
