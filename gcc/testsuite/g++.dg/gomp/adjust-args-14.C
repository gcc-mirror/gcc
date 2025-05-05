/* Make sure non-dependent nothing list items are not removed too early.
   The order of the adjust_args clauses is important for this test, there was a
   bug where we were removing nothing list-items before we encountered a
   dependent item.  */

template<int>
struct S {};

template<typename T>
void v0 (T) {}

/* { dg-note "parameter previously specified here" "" { target *-*-* } .+3 } */
/* { dg-error "expansion of numeric range specifies non-unique index 1" "" { target *-*-* } .+3 } */
#pragma omp declare variant(v0) match(construct={dispatch}) \
				adjust_args(nothing: 1) \
				adjust_args(nothing: V+0:V+0)
template<int V>
void b0 (S<V>) {}

void f ()
{
  #pragma omp dispatch
  b0 (S<1>());
}
