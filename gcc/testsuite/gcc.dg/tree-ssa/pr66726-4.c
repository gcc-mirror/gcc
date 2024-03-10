/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt1-details" } */

#define SAT(x) (x < 0 ? 0 : (x > 255 ? 255 : x))

void
foo (unsigned char *p, int i)
{
  *p = SAT (i);
}

/* fold could optimize SAT before phiopt1 so only match on the
   MIN/MAX here.  */
/* { dg-final { scan-tree-dump-times "= MIN_EXPR" 1 "phiopt1" } } */
/* { dg-final { scan-tree-dump-times "= MAX_EXPR" 1 "phiopt1" } } */
