/* { dg-do compile } */
/* { dg-options "-march=rv32imafdc_zfa -mabi=ilp32d" { target { rv32 } } } */
/* { dg-options "-march=rv64imafdc_zfa -mabi=lp64d" { target { rv64 } } } */

extern float a;
extern double b;

void foo (float *x, double *y)
{
  {
    *x = __builtin_roundf (a);
    *y = __builtin_round (b);
  }
  {
    *x = __builtin_floorf (a);
    *y = __builtin_floor (b);
  }
  {
    *x = __builtin_ceilf (a);
    *y = __builtin_ceil (b);
  }
  {
    *x = __builtin_truncf (a);
    *y = __builtin_trunc (b);
  }
  {
    *x = __builtin_roundevenf (a);
    *y = __builtin_roundeven (b);
  }
  {
    *x = __builtin_nearbyintf (a);
    *y = __builtin_nearbyint (b);
  }
  {
    *x = __builtin_rintf (a);
    *y = __builtin_rint (b);
  }
}

/* { dg-final { scan-assembler-times "fround.s" 6 } } */
/* { dg-final { scan-assembler-times "fround.d" 6 } } */
/* { dg-final { scan-assembler-times "froundnx.s" 1 } } */
/* { dg-final { scan-assembler-times "froundnx.d" 1 } } */
