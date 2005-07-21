// Derived from PR22167, which failed on some RISC targets.  The call to
// foo() has two successors, one normal and one exceptional, and both
// successors use &a[0] and x.  Expressions involving &a[0] can be hoisted
// before the call but those involving x cannot.
// { dg-options "-Os" }
// { dg-do run }

int a[4];

struct S {
  S() : x (0) {}
  ~S() { a[0] = x; }
  int x;
};

void
foo (int *x)
{
  if (*x == 1)
    throw 1;
  *x = 1;
}

int
main()
{
  S s;
  foo (&s.x);
  if (a[0] == s.x)
    a[0]++;
  return a[0];
}
