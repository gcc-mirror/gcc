// PR c++/20416.  We correctly constructed the temporary S in foo(),
// but incorrectly destroyed it every time foo() was called.
// { dg-do run }
extern "C" void abort (void);
extern "C" void _exit (int);

int c, exiting;
struct S {
  S() { ++c; }
  S(const S &) { ++c; }
  ~S()
  {
    if (!exiting) abort ();
    _exit (0);
  }
};
void
foo (void)
{
  static const S &s = S();
}
int main ()
{
  if (c != 0)
    abort ();
  foo ();
  foo ();
  if (c != 1)
    abort ();
  exiting = 1;
  return 1;
}
