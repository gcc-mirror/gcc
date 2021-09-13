/* { dg-do run } */
/* { dg-options "-g" } */
/* { dg-xfail-run-if "" { aarch64*-*-* } { "-O2" "-O3" "-Os" } { "-fno-fat-lto-objects" } } */

#include "guality.h"

int a;

int
main (int argc, char *argv[])
{
  int tmp = a;
  int tmp2 = a;
  int tmp3;
  int res;
  GUALCHKVAL (a);
  GUALCHKVAL (tmp);
  GUALCHKVAL (tmp2);
  a = 0;
  tmp3 = tmp2;
  GUALCHKVAL (a);
  GUALCHKVAL (tmp);
  GUALCHKVAL (tmp2);
  GUALCHKVAL (tmp3);
  res = tmp - tmp2 + 1;
  return res;
}
