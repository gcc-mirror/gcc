/* { dg-do compile } */
/* { dg-options "-O -fno-tree-sra -fdump-tree-fre-details" } */

union U {
  float f;
  int i;
};

int foo (union U *p)
{
  union U u;
  p->f = 0.0;
  u = *p;
  return u.i;
}

/* { dg-final { scan-tree-dump "Replaced u.i with 0 in" "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
