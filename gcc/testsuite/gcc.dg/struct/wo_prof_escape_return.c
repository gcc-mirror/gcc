/* { dg-do run } */

#include <stdlib.h>

struct A {
  int d;
};

struct A a;

struct A foo ()
{
  a.d = 5;
  return a;
}

int
main ()
{
  a.d = 0;
  foo ();

  if (a.d != 5)
    abort ();

  return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "is return type of function...Excluded" "ipa_struct_reorg" { xfail *-*-* } } } */
/* { dg-final { cleanup-ipa-dump "*" } } */
