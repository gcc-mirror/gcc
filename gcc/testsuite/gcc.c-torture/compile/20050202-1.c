/* From PR 19578.  */
/* { dg-require-effective-target indirect_calls } */

extern void foo (void) __attribute__((noreturn));

void
g (void)
{
  void (*f) (void) = foo;
  f ();
  f ();
}
