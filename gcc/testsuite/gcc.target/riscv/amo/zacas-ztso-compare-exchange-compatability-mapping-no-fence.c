/*
** Verify that atomic op mappings match the PSABI doc's recommended mapping.
** compare_exchange ops without a seq_cst failure ordering do *not* need a
** leading fence.
*/
/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-add-options riscv_zacas } */
/* { dg-add-options riscv_ztso } */
/* { dg-final { scan-assembler-not "\tfence" } } */

void atomic_compare_exchange_weak_int_seq_cst_relaxed (int *bar, int *baz, int qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 0, __ATOMIC_SEQ_CST, __ATOMIC_RELAXED);
}

void atomic_compare_exchange_weak_int_seq_cst_acquire (int *bar, int *baz, int qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 0, __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}

void atomic_compare_exchange_strong_int_seq_cst_relaxed (int *bar, int *baz, int qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 1, __ATOMIC_SEQ_CST, __ATOMIC_RELAXED);
}

void atomic_compare_exchange_strong_int_seq_cst_acquire (int *bar, int *baz, int qux)
{
  __atomic_compare_exchange_n(bar, baz, qux, 1, __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
