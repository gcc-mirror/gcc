/* Check fmin/fmax expanders for scalars on non-VXE targets.  */

/* { dg-do compile } */
/* { dg-options "-O2 -march=z13 -mzarch -fno-trapping-math" } */
/* { dg-final { scan-assembler-times "jg" 4 } } */

double
dofmax (double d1, double d2)
{
  return __builtin_fmax (d1, d2);
}

double
dofmin (double d1, double d2)
{
  return __builtin_fmin (d1, d2);
}

float
dofmaxf (float f1, float f2)
{
  return __builtin_fmaxf (f1, f2);
}

float
dofminf (float f1, float f2)
{
  return __builtin_fminf (f1, f2);
}
