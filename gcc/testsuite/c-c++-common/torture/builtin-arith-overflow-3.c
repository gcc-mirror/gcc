/* Test __builtin_{add,sub,mul,{s,u}addll,{s,u}subll,{s,u}mulll}_overflow.  */
/* { dg-do run } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */

#include "builtin-arith-overflow-1.h"

#define U(s, op) s##op##ll
TESTS (long long, LLONG_MIN, LLONG_MAX)
#undef U
#define U(s, op) op
TESTS (long long, LLONG_MIN, LLONG_MAX)

#undef T
#define T(n, t1, t2, tr, v1, v2, vr, b, o) t##n##b ();

int
main ()
{
  TESTS (long long, LLONG_MIN, LLONG_MAX)
#undef U
#define U(s, op) s##op##ll
  TESTS (long long, LLONG_MIN, LLONG_MAX)
  return 0;
}
