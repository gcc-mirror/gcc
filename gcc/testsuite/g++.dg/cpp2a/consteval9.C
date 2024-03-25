// { dg-do compile }
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.
// { dg-options "-std=c++2a" }

consteval int bar (int i) { if (i != 1) throw 1; return 0; }	// { dg-error "is not a constant expression" }

template <int N>
void foo ()
{
  int a = bar (N);
}

template <int N>
void qux ()
{
  int a = bar (N);	// { dg-message "in 'constexpr' expansion of 'bar\\(2\\)'" }
// { dg-error "call to consteval function" "" { target *-*-* } .-1 }
}

// This function is not instantiated so NDR.
template <int N>
void quux ()
{
  int a = bar (5);
}

void
baz ()
{
  foo<1> ();
  qux<2> ();
}

int a = bar (2);	// { dg-message "in 'constexpr' expansion of 'bar\\(2\\)'" }
// { dg-error "call to consteval function" "" { target *-*-* } .-1 }
