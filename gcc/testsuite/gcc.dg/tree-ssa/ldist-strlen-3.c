/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-details" } */
/* { dg-additional-options "-march=z13 -mzarch" { target s390x-*-* } } */
/* { dg-final { scan-tree-dump-times "generated strlenSI\n" 1 "ldist" { target s390x-*-* } } } */

extern int s[];

int test ()
{
  int i = 0;
  for (; s[i]; ++i);
  return i;
}
