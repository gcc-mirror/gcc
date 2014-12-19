/* Make sure that we avoid abs.fmt and neg.fmt when the signs of NaNs
   matter.  */
/* { dg-do compile } */
/* { dg-options "isa_rev<=5 -mhard-float -fno-finite-math-only -mabs=legacy" } */
/* { dg-final { scan-assembler-not "\tneg.s\t" } } */
/* { dg-final { scan-assembler-not "\tneg.d\t" } } */
/* { dg-final { scan-assembler-not "\tabs.s\t" } } */
/* { dg-final { scan-assembler-not "\tabs.d\t" } } */

float f1 (float f) { return -f; }
float f2 (float f) { return __builtin_fabsf (f); }
double d1 (double d) { return -d; }
double d2 (double d) { return __builtin_fabs (d); }
