/* { dg-do compile } */
/* { dg-options "-Ofast -march=armv8-a" } */

__attribute__ ((__simd__ ("notinbranch"), const))
double cos (double);

void foo (float *a, double *b)
{
    for (int i = 0; i < 12; i+=3)
      {
        b[i] = cos (5.0 * a[i]);
        b[i+1] = cos (5.0 * a[i+1]);
        b[i+2] = cos (5.0 * a[i+2]);
      }
}

/* { dg-final { scan-assembler-times {bl\t_ZGVnN2v_cos} 6 } } */
