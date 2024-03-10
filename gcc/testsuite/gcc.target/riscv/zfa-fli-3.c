/* { dg-do compile } */
/* { dg-options "-march=rv64imfd_zfa -mabi=lp64d"  { target { rv64 } } } */
/* { dg-options "-march=rv32imfd_zfa -mabi=ilp32d" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-O1" "-Os" "-Og" "-Oz"} } */

double
foo_positive_s (float a)
{
  /* Use 3 FLI FP constants (but type conversion occur in the middle).  */
  return (2.5f * a - 1.0) / 0.875;
}

/* { dg-final { scan-assembler-times "fli\\.s\t" 1 } } */
/* { dg-final { scan-assembler-times "fli\\.d\t" 2 } } */
