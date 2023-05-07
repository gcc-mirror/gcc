/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt1-details" } */


unsigned char
foo1 (unsigned char *p, int i)
{
  if (i < 0)
    return 0;
  {
    int t = i > 255 ? 255 : i;
    return t;
  }
}
/* testing to see if moving the cast out of the conditional. */

/* { dg-final { scan-tree-dump-times "COND_EXPR .*and PHI .*converted to straightline code" 1 "phiopt1" } } */
