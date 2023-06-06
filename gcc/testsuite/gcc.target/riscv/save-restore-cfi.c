/* { dg-do compile } */
/* { dg-options "-g -Os -march=rv32imafc -mabi=ilp32f -msave-restore -mcmodel=medlow" } */
/* { dg-skip-if "" { *-*-* } {"-O2" "-O1" "-O0" "-Og" "-O3" "-Oz" "-flto"} } */
/* { dg-final { scan-assembler-times {\.cfi_def_cfa_offset 16} 2} } */
/* { dg-final { scan-assembler-times {\.cfi_def_cfa_offset 32} 1} } */
/* { dg-final { scan-assembler-times {\.cfi_def_cfa_offset 0} 1} } */

char my_getchar();
float getf();

int foo()
{
  int s0 = my_getchar();
  float f0 = getf();
  int b = my_getchar();
  return f0 + s0 + b;
}
