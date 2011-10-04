/* { dg-require-effective-target sync_char_short } */

/* { dg-message "note: '__sync_fetch_and_nand' changed semantics in GCC 4.4" "" { target *-*-* } 0 } */

void
foo (short *x)
{
  __sync_val_compare_and_swap (x, 1, 0);
  __sync_bool_compare_and_swap (x, 1, 0);
  __sync_lock_test_and_set (x, 0);

  __sync_fetch_and_add (x, 0);
  __sync_fetch_and_add (x, 0);
  __sync_fetch_and_add (x, 0);
  __sync_fetch_and_sub (x, 0);
  __sync_fetch_and_and (x, 0);
  __sync_fetch_and_or (x, 0);
  __sync_fetch_and_xor (x, 0);
  __sync_fetch_and_nand (x, 0);

  __sync_add_and_fetch (x, 0);
  __sync_add_and_fetch (x, 0);
  __sync_add_and_fetch (x, 0);
  __sync_sub_and_fetch (x, 0);
  __sync_and_and_fetch (x, 0);
  __sync_or_and_fetch (x, 0);
  __sync_xor_and_fetch (x, 0);
  __sync_nand_and_fetch (x, 0);
}
