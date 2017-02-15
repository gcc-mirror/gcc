// PR c++/79377
// { dg-do run }
// { dg-options "-fpermissive" }

struct A
{
  A () : a (0) {}
  A& operator++ () { ++a; ++c; return *this; }
  int a;
  static int c;
};

int A::c = 0;

template <typename>
void
foo (A& a)
{
  a++;		// { dg-warning "trying prefix operator instead" }
  if (A::c != 3 || a.a != 3) __builtin_abort ();
  ++a;
  if (A::c != 4 || a.a != 4) __builtin_abort ();
}

int
main ()
{
  A a;
  if (A::c != 0 || a.a != 0) __builtin_abort ();
  ++a;
  if (A::c != 1 || a.a != 1) __builtin_abort ();
  a++;		// { dg-warning "trying prefix operator instead" }
  if (A::c != 2 || a.a != 2) __builtin_abort ();
  foo<int> (a);
  if (A::c != 4 || a.a != 4) __builtin_abort ();
}
