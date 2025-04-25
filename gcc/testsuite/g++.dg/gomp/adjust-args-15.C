/* { dg-do compile { target c++11 } } */

/* Make sure non-dependent nothing list items are not removed too early.
   The order of the adjust_args clauses is important for this test, there was a
   bug where we were removing nothing list-items before we encountered a
   dependent item.  */

template<typename... Ts>
void v1 (int, Ts...) {}

/* { dg-note "parameter previously specified here" "" { target *-*-* } .+3 } */
/* { dg-error "expansion of numeric range specifies non-unique index 1" "" { target *-*-* } .+3 } */
#pragma omp declare variant(v1) match(construct={dispatch}) \
				adjust_args(nothing: 1) \
				adjust_args(nothing: 1:omp_num_args)
template<typename... Ts>
void b1 (int, Ts...) {}

void f ()
{
  #pragma omp dispatch
  b1 (42, 42);
}
