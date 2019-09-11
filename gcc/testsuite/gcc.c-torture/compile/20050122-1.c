/* From PR 19484.  */
/* { dg-require-effective-target indirect_calls } */

extern void foo (void) __attribute__((noreturn));
int n;

void
g (void)
{
  void (*f) (void) = foo;
  if (n)
    f ();
  n = 1;
}
