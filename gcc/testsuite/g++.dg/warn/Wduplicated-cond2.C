// PR c++/105035
// { dg-do compile }
// { dg-options "-Wduplicated-cond" }

class A {
  struct B { int c; int f; } e;
  template <typename> void foo ();
  void bar ();
};

template <typename> void
A::foo ()
{
  int g;
  if (&g == &e.c)
    ;
  else if (&g == &e.f)
    ;
}

void
A::bar ()
{
  int g;
  if (&g == &e.c)	// { dg-message "previously used here" }
    ;
  else if (&g == &e.c)	// { dg-warning "duplicated 'if' condition" }
    ;
}
