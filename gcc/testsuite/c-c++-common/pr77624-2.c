/* PR middle-end/77624 */
/* { dg-do compile } */

void
foo (int *a)
{
  double b = 0;
  __atomic_is_lock_free (2, a, 2);	/* { dg-error "too many arguments" } */
  __atomic_is_lock_free (2);		/* { dg-error "too few arguments" } */
  __atomic_is_lock_free (2, b);		/* { dg-error "incompatible type" "" { target c } } */
					/* { dg-message "expected" "" { target c } .-1 } */
					/* { dg-error "convert" "" { target c++ } .-2 } */
  __atomic_is_lock_free (2, 0);
}

void
bar (int *a)
{
  double b = 0;
  __atomic_always_lock_free (2, a, 2);	/* { dg-error "too many arguments" } */
  __atomic_always_lock_free (2);	/* { dg-error "too few arguments" } */
  __atomic_always_lock_free (2, b);	/* { dg-error "incompatible type" "" { target c } } */
					/* { dg-message "expected" "" { target c } .-1 } */
					/* { dg-error "convert" "" { target c++ } .-2 } */
  __atomic_always_lock_free (2, 0);
}
