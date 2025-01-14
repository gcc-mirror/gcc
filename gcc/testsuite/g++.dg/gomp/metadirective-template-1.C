// { dg-do compile }
// { dg-additional-options "-fdump-tree-gimple" }

// Check that expressions in metadirectives are handled correctly in template
// functions when they are value-dependent and/or type-dependent.

int x;

// Type-dependent expression.
template <typename T>
void
f1 (void)
{
#pragma omp metadirective			\
  when (user={condition(static_cast<T> (1.0))}: target)
  x = 1;
}

// Value-dependent expression.
template <int N>
void
f2 (void)
{
#pragma omp metadirective			\
  when (user={condition(N)}: target)
  x = 2;
}

// Both type- and value-dependent.
template <typename T, T N>
void
f3 (void)
{
#pragma omp metadirective			\
  when (user={condition(N)}: target)
  x = 3;
}

// Expression is itself a template instantiation.
template <typename T, T N>
bool
test (void)
{
  return N != 0;
}

template <typename T, T N>
void
f4 (void)
{
#pragma omp metadirective			\
  when (user={condition(test<T, N> ())}: teams)
  x = 4;
}

int
main (void)
{
  f1 <int> ();
  f2 <1> ();
  f2 <0> ();
  f3 <int, 1> ();
  f3 <int, 0> ();
  f4 <int, 1> ();
  f4 <int, 0> ();
}

// Each of f1..f3 should be instantiated once with a condition expression
// that is a constant 1, producing a target construct.  The metadirective in
// f4 has a non-constant condition expression, so both instantiations will
// produce a conditional including an alternative including a teams construct.

// { dg-final { scan-tree-dump-times "pragma omp target" 3 "gimple" } }
// { dg-final { scan-tree-dump-times "pragma omp teams" 2 "gimple" } }
