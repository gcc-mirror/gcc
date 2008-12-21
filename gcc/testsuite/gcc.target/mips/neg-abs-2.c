/* Make sure that we avoid abs.fmt and neg.fmt when the signs of NaNs
   matter.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mhard-float -fno-finite-math-only" } */
/* { dg-final { scan-assembler-not "neg.s" } } */
/* { dg-final { scan-assembler-not "neg.d" } } */
/* { dg-final { scan-assembler-not "abs.s" } } */
/* { dg-final { scan-assembler-not "abs.d" } } */

float f1 (float f) { return -f; }
float f2 (float f) { return __builtin_fabsf (f); }
double d1 (double d) { return -d; }
double d2 (double d) { return __builtin_fabs (d); }
