/* { dg-do compile } */
/* { dg-options "-g -Os -march=rv32e -mabi=ilp32e -msave-restore" } */
/* { dg-skip-if "" { *-*-* } {"-O2" "-O1" "-O0" "-Og" "-O3" "-Oz" "-flto"} } */
/* { dg-final { scan-assembler-times {\.cfi_offset 8, -8} 1} } */
/* { dg-final { scan-assembler-times {\.cfi_offset 1, -4} 1} } */
/* { dg-final { scan-assembler-times {\.cfi_def_cfa_offset 8} 1} } */
/* { dg-final { scan-assembler-times {\.cfi_restore 8} 1} } */
/* { dg-final { scan-assembler-times {\.cfi_restore 1} 1} } */
/* { dg-final { scan-assembler-times {\.cfi_def_cfa_offset 0} 1} } */

int my_getint();

int foo(int x)
{
  return x + my_getint();
}
