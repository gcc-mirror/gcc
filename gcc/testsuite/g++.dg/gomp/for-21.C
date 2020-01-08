// { dg-do compile { target c++17 } }

void
f1 (int a[10][10])
{
  #pragma omp for collapse (2)
  for (int i = 0; i < 10; ++i)
    for (auto j : a[i])		// { dg-error "initializer expression refers to iteration variable 'i'" }
      ;
}

void
f2 (int (&a)[10])
{
  #pragma omp for collapse (2)
  for (auto i : a)
    for (int j = i * 2; j < i * 4; j++)		// { dg-error "initializer expression refers to iteration variable 'i'" }
      ;
}

struct S { int a, b, c; };

void
f3 (S (&a)[10])
{
  #pragma omp for collapse (2)
  for (auto [i, j, k] : a)			// { dg-error "use of 'i' before deduction of 'auto'" "" { target *-*-* } .+1 }
    for (int l = i; l < j; l += k)		// { dg-error "use of 'j' before deduction of 'auto'" }
      ;						// { dg-error "use of 'k' before deduction of 'auto'" "" { target *-*-* } .-1 }
}

template <int N>
void
f4 (int a[10][10])
{
  #pragma omp for collapse (2)
  for (int i = 0; i < 10; ++i)
    for (auto j : a[i])		// { dg-error "initializer expression refers to iteration variable 'i'" }
      ;
}

template <int N>
void
f5 (int (&a)[10])
{
  #pragma omp for collapse (2)
  for (auto i : a)
    for (int j = i * 2; j < i * 4; j++)		// { dg-error "initializer expression refers to iteration variable 'i'" }
      ;
}

template <int N>
void
f6 (S (&a)[10])
{
  #pragma omp for collapse (2)
  for (auto [i, j, k] : a)			// { dg-error "use of 'i' before deduction of 'auto'" "" { target *-*-* } .-1 }
    for (int l = i; l < j; l += k)		// { dg-error "use of 'j' before deduction of 'auto'" }
      ;						// { dg-error "use of 'k' before deduction of 'auto'" "" { target *-*-* } .-3 }
}

template <typename T>
void
f7 (T a[10][10])
{
  #pragma omp for collapse (2)
  for (T i = 0; i < 10; ++i)
    for (auto j : a[i])		// { dg-error "initializer expression refers to iteration variable 'i'" }
      ;
}

template <typename T>
void
f8 (T (&a)[10])
{
  #pragma omp for collapse (2)
  for (auto i : a)
    for (T j = i * 2; j < i * 4; j++)		// { dg-error "initializer expression refers to iteration variable 'i'" }
      ;
}

template <typename T, typename U>
void
f9 (U (&a)[10])
{
  #pragma omp for collapse (2)
  for (auto [i, j, k] : a)			// { dg-error "use of 'i' before deduction of 'auto'" "" { target *-*-* } .-1 }
    for (T l = i; l < j; l += k)		// { dg-error "use of 'j' before deduction of 'auto'" }
      ;						// { dg-error "use of 'k' before deduction of 'auto'" "" { target *-*-* } .-3 }
}

void
test ()
{
  int a[10][10] {};
  int b[10] {};
  S c[10] {};
  f4 <0> (a);
  f5 <0> (b);
  f6 <0> (c);
  f7 (a);
  f8 (b);
  f9 <int, S> (c);
}
