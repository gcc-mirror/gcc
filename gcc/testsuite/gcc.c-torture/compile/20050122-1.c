/* From PR 19484.  */
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
