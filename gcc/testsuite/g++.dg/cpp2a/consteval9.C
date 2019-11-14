// { dg-do compile }
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
}

template <int N>
void quux ()
{
  int a = bar (5);	// { dg-message "in 'constexpr' expansion of 'bar\\(5\\)'" }
}

void
baz ()
{
  foo<1> ();
  qux<2> ();
}

int a = bar (2);	// { dg-message "in 'constexpr' expansion of 'bar\\(2\\)'" }
