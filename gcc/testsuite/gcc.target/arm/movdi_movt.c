/* { dg-do compile { target { arm_cortex_m && { arm_thumb2_ok || arm_thumb1_movt_ok } } } } */
/* { dg-skip-if "-mslow-flash-data and -mword-relocations incompatible" { *-*-* } { "-mword-relocations" } } */
/* { dg-options "-O2 -mslow-flash-data" } */

unsigned long long
movdi_1 (int a)
{
  return 0xF0F00000LLU;
}

unsigned long long
movdi_2 (int a)
{
  return 0xF0F0000000000000LLU;
}

/* Accept r1 because big endian targets put the low bits in the highest
   numbered register of a pair.  */
/* { dg-final { scan-assembler-times "movt\tr\[01\], 61680" 2 } } */
