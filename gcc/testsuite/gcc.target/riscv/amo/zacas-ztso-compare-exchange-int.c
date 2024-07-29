/* Verify that atomic op mappings match the PSABI doc's recommended mapping.  */
/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-add-options riscv_zacas } */
/* { dg-add-options riscv_ztso } */
/* { dg-final { scan-assembler-not "\tlr\.w" } } */
/* { dg-final { scan-assembler-not "\tsc\.w" } } */
/* { dg-final { scan-assembler-times "amocas\.w\t" 8 } } */

void atomic_compare_exchange_weak_int_relaxed (int *bar, int *baz, int qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
}

void atomic_compare_exchange_strong_int_relaxed (int *bar, int *baz, int qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 1, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
}

void atomic_compare_exchange_weak_int_acquire (int *bar, int *baz, int qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 0, __ATOMIC_ACQUIRE, __ATOMIC_ACQUIRE);
}

void atomic_compare_exchange_strong_int_acquire (int *bar, int *baz, int qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 1, __ATOMIC_ACQUIRE, __ATOMIC_ACQUIRE);
}

void atomic_compare_exchange_weak_int_release (int *bar, int *baz, int qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 0, __ATOMIC_RELEASE, __ATOMIC_RELAXED);
}

void atomic_compare_exchange_strong_int_release (int *bar, int *baz, int qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 1, __ATOMIC_RELEASE, __ATOMIC_RELAXED);
}

void atomic_compare_exchange_weak_int_acq_rel (int *bar, int *baz, int qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 0, __ATOMIC_ACQ_REL, __ATOMIC_RELAXED);
}

void atomic_compare_exchange_strong_int_acq_rel (int *bar, int *baz, int qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 1, __ATOMIC_ACQ_REL, __ATOMIC_RELAXED);
}
