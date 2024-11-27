/* PR target/117642 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-mno-cx16" } */
/* { dg-final { scan-assembler "__sync_lock_test_and_set_16" } } */
/* { dg-final { scan-assembler "__sync_lock_release_16" } } */

__int128 t = 1;

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
