/* Related to PR 19484.  */
/* { dg-require-effective-target trampolines } */

extern void foo (void) __attribute__((noreturn));
int n;

void
g (void)
{
  __label__ lab;
  void h (void) { if (n == 2) goto lab; }
  void (*f1) (void) = foo;
  void (*f2) (void) = h;

  f2 ();
  if (n)
    f1 ();
  n = 1;
 lab:
  n++;
}
