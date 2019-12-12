/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-ifcvt-details -fdump-tree-vect" } */

int a, b, c, *e;
int d[2];

void f ()
{
  while (c)
    {
      d[0] = 4;
      d[1] = 4;
      *e = b == 0 ? 0 : a / b;
    }
}

/* { dg-final { scan-tree-dump "COND_DIV" "ifcvt" } } */
/* { dg-final { scan-tree-dump-not "COND_DIV" "vect" } } */
