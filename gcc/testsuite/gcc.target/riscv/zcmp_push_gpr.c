/* { dg-do compile } */
/* { dg-options "-march=rv32imafd_zicsr_zifencei_zca_zcmp -mabi=ilp32d -Os -g" } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-O1" "-O2" "-Og" "-O3" "-Oz" "-flto"} } */

int
zcmp_push ()
{
  __asm__ __volatile__("" ::: "ra", "s0", "s2");
  return 0;
}

/* { dg-final { scan-assembler ".cfi_offset 1, -16\n\t.cfi_offset 8, -12\n\t.cfi_offset 9, -8\n\t.cfi_offset 18, -4\n\t.cfi_def_cfa_offset 16" } } */
