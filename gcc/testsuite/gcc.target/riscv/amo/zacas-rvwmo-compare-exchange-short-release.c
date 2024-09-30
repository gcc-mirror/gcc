/* Verify that atomic op mappings match the PSABI doc's recommended mapping.  */
/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-add-options riscv_zabha } */
/* { dg-add-options riscv_zacas } */
/* { dg-remove-options riscv_ztso } */
/* { dg-final { scan-assembler-not "\tlr\.w" } } */
/* { dg-final { scan-assembler-not "\tsc\.w" } } */
/* { dg-final { scan-assembler-times "amocas\.h\.rl\t" 2 } } */


void atomic_compare_exchange_weak_short_release (short *bar, short *baz, short qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 0, __ATOMIC_RELEASE, __ATOMIC_RELAXED);
}

void atomic_compare_exchange_strong_short_release (short *bar, short *baz, short qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 1, __ATOMIC_RELEASE, __ATOMIC_RELAXED);
}
