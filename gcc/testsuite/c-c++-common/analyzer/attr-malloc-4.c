/* An example where the deallocator requires non-NULL.  */

struct foo;
extern void foo_release (struct foo *)
  __attribute__((nonnull));
extern struct foo *foo_acquire (void)
  __attribute__ ((malloc (foo_release)));

void test_1 (void)
{
  struct foo *p = foo_acquire (); /* { dg-message "this call could return NULL" } */
  foo_release (p); /* { dg-warning "use of possibly-NULL 'p' where non-null" } */
}

void test_2 (void)
{
  struct foo *p = foo_acquire ();
  if (!p)
    return;
  foo_release (p);
}
