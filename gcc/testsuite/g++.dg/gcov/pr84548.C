// PR gcov-profile/84548
// { dg-options "-fprofile-arcs -ftest-coverage" }
// { dg-do run { target native } }
// TODO: add support for groups to gcov.exp script

struct A { static int foo () { return 1; }; static int bar () {
  int x;
  return 2; } };

int main()
{
  int a = A::foo () + A::bar ();
  if (a != 3)
    return 1;

  return 0;
}

// { dg-final { run-gcov pr84548.C } }
