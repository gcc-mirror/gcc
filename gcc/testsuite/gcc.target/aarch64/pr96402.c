/* PR target/96402 */
/* { dg-do run { target int128 } } */
/* { dg-options "-moutline-atomics" } */

int
main ()
{
  __int128 a = 0;
  __sync_val_compare_and_swap (&a, (__int128) 0, (__int128) 1);
  if (a != 1)
    __builtin_abort ();
  __sync_val_compare_and_swap (&a, (__int128) 1, (((__int128) 0xdeadbeeffeedbac1ULL) << 64) | 0xabadcafe00c0ffeeULL);
  if (a != ((((__int128) 0xdeadbeeffeedbac1ULL) << 64) | 0xabadcafe00c0ffeeULL))
    __builtin_abort ();
  return 0;
}
