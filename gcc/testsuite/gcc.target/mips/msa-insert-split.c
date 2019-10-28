/* { dg-do compile } */
/* { dg-options "-mfp64 -mhard-float -mmsa" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef double v2f64 __attribute__ ((vector_size (16)));

void foo (double* arr, v2f64* vec)
{
  v2f64 v;
  v[0] = arr[0];
  v[1] = arr[1];
  *vec = v;
}

/* { dg-final { scan-assembler-not "insert.w" } } */
/* { dg-final { scan-assembler-times "insve.d" 2 } } */
