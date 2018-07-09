/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce1" } */

long long
foo (long long a, signed char b, signed char c)
{
  int bc = b * c;
  return a + (short)bc;
}

/* EVRP should remove the truncation to short, keeping (long long)bc.  */
/* { dg-final { scan-tree-dump-not "short" "cddce1" { target int32plus } } } */
