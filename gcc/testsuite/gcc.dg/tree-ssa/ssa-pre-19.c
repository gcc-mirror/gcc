/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

struct Loc {
    int x[3];
};

void bar (struct Loc *);

int foo (int i, int j, int k, int b)
{
  struct Loc IND;
  int res;

  if (b)
    {
      IND.x[0] = i;
      IND.x[1] = j;
      IND.x[2] = k-1;
    }
  else
    {
      IND.x[0] = i;
      IND.x[1] = j;
      IND.x[2] = k;
    }

  /* This should be optimized to i + j + {k, k + 1}.  */
  res = IND.x[0] + IND.x[1] + IND.x[2];

  /* This is just to prevent SRA.  */
  bar (&IND);

  return res;
}

/* All three loads should be eliminated.  */
/* { dg-final { scan-tree-dump "Eliminated: 3" "pre" } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
