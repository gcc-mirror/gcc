/* PR c/102989 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=gnu23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 192
#include "../../c-c++-common/torture/builtin-arith-overflow-1.h"

#define U(s, op) op
TESTS (_BitInt(192), (-3138550867693340381917894711603833208051177722232017256447wb - 1), 3138550867693340381917894711603833208051177722232017256447wb)

#undef T
#define T(n, t1, t2, tr, v1, v2, vr, b, o) t##n##b ();
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 192
  TESTS (_BitInt(192), (-3138550867693340381917894711603833208051177722232017256447wb - 1), 3138550867693340381917894711603833208051177722232017256447wb)
#endif
  return 0;
}
