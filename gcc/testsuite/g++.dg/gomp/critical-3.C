int i;

template <int N>
void
foo (void)
{
  #pragma omp critical (foo), hint (N + 1)
  i++;
}

template <int N>
void
bar (void)
{
  #pragma omp critical (bar), hint (N + i)	// { dg-error "constant integer expression" }
  i++;
}

template <typename T>
void
baz (T x)
{
  #pragma omp critical (baz) hint (x)	// { dg-error "expression must be integral" }
  i++;
}

void
test ()
{
  foo <0> ();
  bar <0> ();
  baz (0.0);
}
