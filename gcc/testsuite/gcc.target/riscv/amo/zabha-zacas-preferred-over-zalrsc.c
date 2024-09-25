/* { dg-do compile } */
/* Ensure that AMO ops are emitted for subword cas when both zalrsc and
   zacas/zabha are enabled.  */
/* { dg-options "-O3" } */
/* { dg-add-options riscv_zalrsc } */
/* { dg-add-options riscv_zacas } */
/* { dg-add-options riscv_zabha } */
/* { dg-final { scan-assembler-not "\tlr\.w" } } */
/* { dg-final { scan-assembler-not "\tsc\.w" } } */
/* { dg-final { scan-assembler-times "amocas\.b\t" 2 } } */


void atomic_compare_exchange_weak_char_relaxed (char *bar, char *baz, char qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
}

void atomic_compare_exchange_strong_char_relaxed (char *bar, char *baz, char qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 1, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
}
