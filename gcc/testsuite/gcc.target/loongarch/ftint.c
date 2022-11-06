/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -mdouble-float -fno-math-errno -ffp-int-builtin-inexact" } */
/* { dg-final { scan-assembler "ftint\\.l\\.s" } } */
/* { dg-final { scan-assembler "ftint\\.l\\.d" } } */
/* { dg-final { scan-assembler "ftintrm\\.l\\.s" } } */
/* { dg-final { scan-assembler "ftintrm\\.l\\.d" } } */
/* { dg-final { scan-assembler "ftintrp\\.l\\.s" } } */
/* { dg-final { scan-assembler "ftintrp\\.l\\.d" } } */

long
my_lrint (double a)
{
  return __builtin_lrint (a);
}

long
my_lrintf (float a)
{
  return __builtin_lrintf (a);
}

long
my_lfloor (double a)
{
  return __builtin_lfloor (a);
}

long
my_lfloorf (float a)
{
  return __builtin_lfloorf (a);
}

long
my_lceil (double a)
{
  return __builtin_lceil (a);
}

long
my_lceilf (float a)
{
  return __builtin_lceilf (a);
}
