// PR c++/112795
// { dg-do compile { target c++11 } }

void
foo ()
{
  #pragma GCC unroll 1.0f			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (int i = 0; i < 2; ++i)
    ;
  #pragma GCC unroll 0xffffffffffffffffULL	// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (int i = 0; i < 2; ++i)
    ;
  #pragma GCC unroll -42			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (int i = 0; i < 2; ++i)
    ;
}

template <int N>
void
bar ()
{
  #pragma GCC unroll 1.0f			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (int i = 0; i < 2; ++i)
    ;
  #pragma GCC unroll 0xffffffffffffffffULL	// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (int i = 0; i < 2; ++i)
    ;
  #pragma GCC unroll -42			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (int i = 0; i < 2; ++i)
    ;
}

template <typename T, int N>
void
baz ()
{
  #pragma GCC unroll (N + 1.0f)			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (int i = 0; i < 2; ++i)
    ;
  #pragma GCC unroll (N + 0xffffffffffffffffULL)
  for (int i = 0; i < 2; ++i)
    ;
  #pragma GCC unroll (N - 42)
  for (int i = 0; i < 2; ++i)
    ;
  #pragma GCC unroll ((T) 1.0f)
  for (int i = 0; i < 2; ++i)
    ;
  #pragma GCC unroll ((T) 0xffffffffffffffffULL)
  for (int i = 0; i < 2; ++i)
    ;
  #pragma GCC unroll ((T) -42)
  for (int i = 0; i < 2; ++i)
    ;
}

template <typename T, int N>
void
qux ()
{
  #pragma GCC unroll (N + 1.0f)			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (int i = 0; i < 2; ++i)
    ;
  #pragma GCC unroll (N + 0xffffffffffffffffULL)// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (int i = 0; i < 2; ++i)
    ;
  #pragma GCC unroll (N - 42)			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (int i = 0; i < 2; ++i)
    ;
  #pragma GCC unroll ((T) 1.0f)			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (int i = 0; i < 2; ++i)
    ;
  #pragma GCC unroll ((T) 0xffffffffffffffffULL)// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (int i = 0; i < 2; ++i)
    ;
  #pragma GCC unroll ((T) -42)			// { dg-error "'#pragma GCC unroll' requires an assignment-expression that evaluates to a non-negative integral constant less than" }
  for (int i = 0; i < 2; ++i)
    ;
}

void
corge ()
{
  qux <float, 0> ();
}
