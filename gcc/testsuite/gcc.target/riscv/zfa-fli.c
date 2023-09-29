/* { dg-do compile } */
/* { dg-options "-march=rv32imafdc_zfa -mabi=ilp32d" { target { rv32 } } } */
/* { dg-options "-march=rv64imafdc_zfa -mabi=lp64d" { target { rv64 } } } */

void foo_float32 ()
{
  volatile float a;
  a = -1.0;
  a = 1.1754944e-38;
  a = 1.0/(1 << 16);
  a = 1.0/(1 << 15);
  a = 1.0/(1 << 8);
  a = 1.0/(1 << 7);
  a = 1.0/(1 << 4);
  a = 1.0/(1 << 3);
  a = 1.0/(1 << 2);
  a = 0.3125;
  a = 0.375;
  a = 0.4375;
  a = 0.5;
  a = 0.625;
  a = 0.75;
  a = 0.875;
  a = 1.0;
  a = 1.25;
  a = 1.5;
  a = 1.75;
  a = 2.0;
  a = 2.5;
  a = 3.0;
  a = 1.0*(1 << 2);
  a = 1.0*(1 << 3);
  a = 1.0*(1 << 4);
  a = 1.0*(1 << 7);
  a = 1.0*(1 << 8);
  a = 1.0*(1 << 15);
  a = 1.0*(1 << 16);
  a = __builtin_inff ();
  a = __builtin_nanf ("");
}

void foo_double64 ()
{
  volatile double a;
  a = -1.0;
  a = 2.2250738585072014e-308;
  a = 1.0/(1 << 16);
  a = 1.0/(1 << 15);
  a = 1.0/(1 << 8);
  a = 1.0/(1 << 7);
  a = 1.0/(1 << 4);
  a = 1.0/(1 << 3);
  a = 1.0/(1 << 2);
  a = 0.3125;
  a = 0.375;
  a = 0.4375;
  a = 0.5;
  a = 0.625;
  a = 0.75;
  a = 0.875;
  a = 1.0;
  a = 1.25;
  a = 1.5;
  a = 1.75;
  a = 2.0;
  a = 2.5;
  a = 3.0;
  a = 1.0*(1 << 2);
  a = 1.0*(1 << 3);
  a = 1.0*(1 << 4);
  a = 1.0*(1 << 7);
  a = 1.0*(1 << 8);
  a = 1.0*(1 << 15);
  a = 1.0*(1 << 16);
  a = __builtin_inf ();
  a = __builtin_nan ("");
}

/* { dg-final { scan-assembler-times {\mfli\.s\M} 32 } } */
/* { dg-final { scan-assembler-times {\mfli\.d\M} 32 } } */
