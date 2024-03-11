/* { dg-do compile } */
/* { dg-options "-march=rv32imafdc_zfa_zfh -mabi=ilp32d -O0" { target { rv32 } } } */
/* { dg-options "-march=rv64imafdc_zfa_zfh -mabi=lp64d -O0" { target { rv64 } } } */

void foo_float16 ()
{
  volatile _Float16 a;
  a = -1.0;
  a = 6.104e-5;
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
  a = __builtin_inff16 ();
  a = __builtin_nanf16 ("");
}

/* { dg-final { scan-assembler-times {\mfli\.h\M} 32 } } */
