/* Make sure that we use abs.fmt and neg.fmt when the signs of NaNs don't
   matter.  */
/* { dg-do compile } */
/* { dg-mips-options "-O2 -mhard-float -ffinite-math-only" } */
/* { dg-final { scan-assembler "neg.s" } } */
/* { dg-final { scan-assembler "neg.d" } } */
/* { dg-final { scan-assembler "abs.s" } } */
/* { dg-final { scan-assembler "abs.d" } } */

float f1 (float f) { return -f; }
float f2 (float f) { return __builtin_fabsf (f); }
double d1 (double d) { return -d; }
double d2 (double d) { return __builtin_fabs (d); }
