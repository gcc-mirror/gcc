/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */
/* { dg-require-effective-target dfp } */

/* We do not support DFPs.  */

int foo(short x) {
  /* { dg-final { scan-tree-dump "\\(_Decimal32\\)" "optimized" } } */
  return (_Decimal32) x != 0;
}
