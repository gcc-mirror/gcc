/* { dg-do compile } */
/* Ensure subword zacas is not emitted unless both zacas and zabha are
   present.  */
/* { dg-options "-O3" } */
/* { dg-add-options riscv_zabha } */
/* { dg-remove-options riscv_zacas } */
/* { dg-remove-options riscv_zalrsc } */
/* { dg-final { scan-assembler "\tcall\t" } } */
/* { dg-final { scan-assembler-not "\tlr\.w" } } */
/* { dg-final { scan-assembler-not "\tsc\.w" } } */
/* { dg-final { scan-assembler-not "amocas\.b\t" } } */


void atomic_compare_exchange_char_relaxed (char *bar, char *baz, char qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 1, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
}
