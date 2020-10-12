int i;

template <int N>
void
foo0 (void)
{
  #pragma omp critical (foo), hint (N + 1)  // { dg-error "critical' with 'hint' clause requires a name, except when 'omp_sync_hint_none' is used" }
  i++;
}

template <int N>
void
foo_1 (void)
{
  #pragma omp critical (foo), hint (N + 1)
  i++;
}

template <int N>
void
foobar0 (void)
{
  #pragma omp critical hint (N + 0)
  i++;
}

template <int N>
void
foobar1 (void)
{
  #pragma omp critical hint (N + 0)  // { dg-error "critical' with 'hint' clause requires a name, except when 'omp_sync_hint_none' is used" }
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
  foo0 <0> ();    // Error
  foo_1 <-1> ();  // OK
  foobar0 <0> (); // OK
  foobar1 <1> (); // Error
  bar <0> ();
  baz (0.0);
}
