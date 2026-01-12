/* A simple variant of gcc.dg/torture/bitint-64.c */
/* { dg-do run { target bitint } } */
/* { dg-require-effective-target sync_char_short } */
/* { dg-options "-std=c23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#include "../bitintext.h"

enum E : char { E22 = 22 } e = E22;

int
main ()
{
  _Atomic _BitInt (5) b = 0;
  b += e;
  BEXTC (b);
  return 0;
}
