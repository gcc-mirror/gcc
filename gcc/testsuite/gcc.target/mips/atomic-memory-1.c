/* { dg-do run } */

/* { dg-message "note: '__sync_nand_and_fetch' changed semantics in GCC 4.4" "" { target *-*-* } 0 } */

extern void abort (void);
extern void exit (int);

NOMIPS16 int main ()
{
#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_4
  unsigned v = 0;
  __sync_synchronize ();

  if (!__sync_bool_compare_and_swap (&v, 0, 30000))
    abort();
  if (30000 != __sync_val_compare_and_swap (&v, 30000, 100001))
    abort();
  __sync_sub_and_fetch (&v, 0x8001);
  __sync_sub_and_fetch (&v, 0x7fff);
  if (v != 34465)
    abort();
  if (__sync_nand_and_fetch (&v, 0xff) != -162)
    abort();
  if (__sync_fetch_and_add (&v, 262) != -162)
    abort();
  if (v != 100)
    abort();
  if (__sync_or_and_fetch (&v, 0xf001) != 0xf065)
    abort();
  if (__sync_and_and_fetch (&v, 0x1000) != 0x1000)
    abort();
  if (__sync_xor_and_fetch (&v, 0xa51040) != 0xa50040)
    abort();
  __sync_and_and_fetch (&v, 7);
  if (__sync_lock_test_and_set(&v, 1) != 0)
    abort();
  if (v != 1)
    abort();
  __sync_lock_release (&v);
  if (v != 0)
    abort();
#endif
  exit(0);
}
