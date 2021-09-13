/* Example of extra argument to "malloc" attribute.  */

struct foo;
extern void foo_release (int, struct foo *);
extern struct foo *foo_acquire (void)
  __attribute__ ((malloc (foo_release, 2)));

void test_1 (void)
{
  struct foo *p = foo_acquire ();
  foo_release (0, p);
}
