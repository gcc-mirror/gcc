/* PR target/117642 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-march=i486" } */
/* { dg-final { scan-assembler "__sync_lock_test_and_set_8" } } */
/* { dg-final { scan-assembler "__sync_lock_release_8" } } */

long long t = 1;

void
foo (void)
{
  __sync_lock_test_and_set (&t, 1);
}

void
bar (void)
{
  __sync_lock_release (&t);
}
