/* PR tree-optimization/92683 - strncmp incorrect result with equal substrings
   and nonconst bound
   { dg-do run }
   { dg-options "-O2 -Wall" } */

#include "strlenopt.h"

#define ident(n) ident (n)

__attribute__ ((noclone, noinline, noipa)) size_t
ident (size_t x)
{
  return x;
}

int nfails;

__attribute__ ((noclone, noinline, noipa)) void
failure_on_line (int line)
{
  __builtin_printf ("failure on line %i\n", line);
  ++nfails;
}

#include "strcmpopt_8.c"

int main (void)
{
  test_literal ();
  test_cst_array ();

  if (nfails)
    __builtin_abort ();
}
