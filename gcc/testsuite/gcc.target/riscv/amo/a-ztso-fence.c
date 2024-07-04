/* { dg-do compile } */
/* Verify that fence mappings match the PSABI doc's recommended mapping.  */
/* { dg-options "-O3" } */
/* { dg-add-options riscv_ztso } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** fence_relaxed:
**	ret
*/
void fence_relaxed()
{
  __atomic_thread_fence(__ATOMIC_RELAXED);
}

/*
** fence_acquire:
**	ret
*/
void fence_acquire()
{
  __atomic_thread_fence(__ATOMIC_ACQUIRE);
}

/*
** fence_release:
**	ret
*/
void fence_release()
{
  __atomic_thread_fence(__ATOMIC_RELEASE);
}

/*
** fence_acq_rel:
**	ret
*/
void fence_acq_rel()
{
  __atomic_thread_fence(__ATOMIC_ACQ_REL);
}

/*
** fence_seq_cst:
**	fence\trw,rw
**	ret
*/
void fence_seq_cst()
{
  __atomic_thread_fence(__ATOMIC_SEQ_CST);
}
