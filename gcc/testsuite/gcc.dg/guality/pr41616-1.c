/* { dg-do run { xfail *-*-* } } */
/* { dg-options "-g" } */

#include "guality.h"

inline int f(int *a)
{
  return *a;
}

int
main(int argc, char *argv[])
{
  int b = -1;
  GUALCHKVAL (b);
  if (argc > 0)
    b = -f(&b);
  GUALCHKVAL (b);
  return b;
}
