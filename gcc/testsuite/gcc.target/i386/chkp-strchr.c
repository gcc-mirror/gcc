/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2" } */

#include "string.h"

static char *
test1 (char *str)
{
  return strrchr (str, '_');
}

char *
test2 ()
{
  return test1 ("test_string");
}
