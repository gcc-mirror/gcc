/* PR target/114310 */
/* { dg-do run { target int128 } } */

volatile __attribute__((aligned (sizeof (__int128_t)))) __int128_t v = 10;

int
main ()
{
#if __GCC_HAVE_SYNC_COMPARE_AND_SWAP_16
  if (__sync_val_compare_and_swap (&v, (__int128_t) 10, (__int128_t) 0) != 10)
    __builtin_abort ();
  if (__sync_val_compare_and_swap (&v, (__int128_t) 10, (__int128_t) 15) != 0)
    __builtin_abort ();
  if (__sync_val_compare_and_swap (&v, (__int128_t) 0, (__int128_t) 42) != 0)
    __builtin_abort ();
  if (__sync_val_compare_and_swap (&v, (__int128_t) 31, (__int128_t) 35) != 42)
    __builtin_abort ();
#endif
  return 0;
}
