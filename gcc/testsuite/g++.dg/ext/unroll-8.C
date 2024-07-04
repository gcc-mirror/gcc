// PR c++/112795
// { dg-do compile { target c++11 } }

void
foo (int (&a)[3])
{
  #pragma GCC unroll 1.0f			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (auto i : a)
    ;
  #pragma GCC unroll 0xffffffffffffffffULL	// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (auto i : a)
    ;
  #pragma GCC unroll -42			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (auto i : a)
    ;
}

template <int N, typename U>
void
bar (U a)
{
  #pragma GCC unroll 1.0f			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (auto i : a)
    ;
  #pragma GCC unroll 0xffffffffffffffffULL	// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (auto i : a)
    ;
  #pragma GCC unroll -42			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (auto i : a)
    ;
}

template <typename T, int N, typename U>
void
baz (U a)
{
  #pragma GCC unroll (N + 1.0f)			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (auto i : a)
    ;
  #pragma GCC unroll (N + 0xffffffffffffffffULL)
  for (auto i : a)
    ;
  #pragma GCC unroll (N - 42)
  for (auto i : a)
    ;
  #pragma GCC unroll ((T) 1.0f)
  for (auto i : a)
    ;
  #pragma GCC unroll ((T) 0xffffffffffffffffULL)
  for (auto i : a)
    ;
  #pragma GCC unroll ((T) -42)
  for (auto i : a)
    ;
}

template <typename T, int N, typename U>
void
qux (U a)
{
  #pragma GCC unroll (N + 1.0f)			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (auto i : a)
    ;
  #pragma GCC unroll (N + 0xffffffffffffffffULL)// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (auto i : a)
    ;
  #pragma GCC unroll (N - 42)			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (auto i : a)
    ;
  #pragma GCC unroll ((T) 1.0f)			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (auto i : a)
    ;
  #pragma GCC unroll ((T) 0xffffffffffffffffULL)// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (auto i : a)
    ;
  #pragma GCC unroll ((T) -42)			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (auto i : a)
    ;
}

void
corge ()
{
  int a[3] = { 1, 2, 3 };
  qux <float, 0, int (&)[3]> (a);
}
