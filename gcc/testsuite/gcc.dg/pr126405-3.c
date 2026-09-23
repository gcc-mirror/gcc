/* { dg-do compile } */
/* { dg-options "-O2 -ffloat-store -ftree-coalesce-vars -fdump-rtl-expand" } */

/* Coalescing puts names of u and of v in one partition, and its representative
   is a name of v rather than the lowest numbered member.  The variable that
   expansion attaches to that partition is therefore the one of the
   representative, and keying the split on any other member of the partition
   leaves it and the second partition of v sharing a MEM_EXPR.  */

double g1, g2, g3, g4, g5;

void
f (int n)
{
  double u = g1;
  double v = u;
  for (int i = 0; i < n; i++)
    v = v + g3;
  double t = v;
  v = g2;
  g4 = t;
  g5 = v;
}

/* { dg-final { scan-rtl-dump {\[[0-9]+ v\+0} "expand" } } */
/* { dg-final { scan-rtl-dump {\[[0-9]+ D\.[0-9]+\+0} "expand" } } */
