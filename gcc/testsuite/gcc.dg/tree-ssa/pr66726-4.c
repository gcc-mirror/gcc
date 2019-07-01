/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt1-details" } */

#define SAT(x) (x < 0 ? 0 : (x > 255 ? 255 : x))

void
foo (unsigned char *p, int i)
{
  *p = SAT (i);
}

/* { dg-final { scan-tree-dump-times "COND_EXPR .*and PHI .*converted to straightline code" 1 "phiopt1" } } */
