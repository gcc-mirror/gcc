/* PR c/117641 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-std=c23" } */

void
foo (_BitInt(128) *b)
{
  __sync_add_and_fetch (b, 1);			/* { dg-error "incompatible" "" { target { ! int128 } } } */
  __sync_val_compare_and_swap (b, 0, 1);	/* { dg-error "incompatible" "" { target { ! int128 } } } */
  __sync_bool_compare_and_swap (b, 0, 1);	/* { dg-error "incompatible" "" { target { ! int128 } } } */
  __sync_lock_test_and_set (b, 1);		/* { dg-error "incompatible" "" { target { ! int128 } } } */
  __sync_lock_release (b);			/* { dg-error "incompatible" "" { target { ! int128 } } } */
}
