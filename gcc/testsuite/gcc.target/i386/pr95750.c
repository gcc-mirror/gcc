/* PR target/95750 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=core2" } */

void
foo (void)
{
  __atomic_thread_fence (__ATOMIC_SEQ_CST);
}

int x;

void
bar (void)
{
  __atomic_store_n (&x, -1, __ATOMIC_SEQ_CST);
}

/* { dg-final { scan-assembler-not "mfence" } } */
