/* PR c/71514 */
/* { dg-do compile } */
/* { dg-options "" } */

void
foo ()
{
}

int a, b;

void
fn1 (void)
{
  __atomic_exchange (&a, &foo, &b, __ATOMIC_RELAXED); /* { dg-error "must not be a pointer to a function" } */
}

void
fn2 (int n)
{
  int arr[n];
  __atomic_exchange (&a, &arr, &b, __ATOMIC_RELAXED); /* { dg-error "must be a pointer to a constant size type" } */
}
