/* { dg-skip-if "" { powerpc*-*-aix* } } */

#include <stdio.h>
#include "analyzer-decls.h"

int test_1 (void)
{
  int c = getchar ();
  return c;
}

int glob_2;
int test_2 (void)
{
  int c;
  glob_2 = 42;
  __analyzer_eval (glob_2 == 42); /* { dg-warning "TRUE" } */
  c = getchar ();
  __analyzer_eval (glob_2 == 42); /* { dg-warning "TRUE" } */
  return c;
}
