/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-evrp" } */

/* The index is [0, 2] so the range for v_3 should be [0, 0][5, 5].  */

static const int tbl[8] = { [2] = 5, [5] = 9 };

int __GIMPLE (ssa,startwith("evrp"))
foo (unsigned int i)
{
  int v;

  __BB(2):
  if (i_2(D) > 2u)
    goto __BB4;
  else
    goto __BB3;

  __BB(3):
  v_3 = tbl[i_2(D)];
  return v_3;

  __BB(4):
  return 0;
}

/* { dg-final { scan-tree-dump "v_3 = .irange. int .0, 0..5, 5.\n" "evrp" } } */
