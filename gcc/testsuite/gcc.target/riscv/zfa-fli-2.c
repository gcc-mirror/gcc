/* { dg-do compile } */
/* { dg-options "-march=rv64imfd_zfa -mabi=lp64d"  { target { rv64 } } } */
/* { dg-options "-march=rv32imfd_zfa -mabi=ilp32d" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-O1" "-O2" "-Os" "-Og" "-Oz"} } */

#ifndef __riscv_zfa
#error Feature macro not defined
#endif

double
foo_negative_d (double a)
{
  /* Use 3 "non-FLI" FP constants.  */
  return (3.5 * a - 5.0) / 0.1875;
}

float
foo_negative_s (float a)
{
  return ((float) 3.5 * a - (float) 5.0) / (float) 0.1875;
}

/* { dg-final { scan-assembler-not "fli\\.s\t" } } */
/* { dg-final { scan-assembler-not "fli\\.d\t" } } */
