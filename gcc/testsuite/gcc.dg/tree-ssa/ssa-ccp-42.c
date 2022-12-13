/* { dg-do compile } */
/* { dg-options "-fgimple -O -fdump-tree-ccp1" } */

__GIMPLE (ssa,startwith("ccp")) int foo (int n)
{
  int i;
  int j;

  __BB(2):
    i_1 = 0;
    goto __BB3;

  __BB(3):
    i_2 = __PHI (__BB2: i_1, __BB3: i_4);
    j_3 = i_2;
    i_4 = i_2 + 1;
    if (i_4 < n_5(D))
      goto __BB3;
    else
      goto __BB4;

  __BB(4):
    return j_3;
}

/* { dg-final { scan-tree-dump "return i_2;" "ccp1" } } */
