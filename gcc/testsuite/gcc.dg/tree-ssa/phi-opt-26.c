/* { dg-do compile } */
/* { dg-options "-O -fgimple -fdump-tree-phiopt1" } */

int __GIMPLE (ssa,startwith("phiopt"))
foo (int a, int b, int flag)
{
  int res;

  __BB(2):
  if (flag_2(D) != 0)
    goto __BB6;
  else
    goto __BB4;

  __BB(4):
  if (a_3(D) > b_4(D))
    goto __BB7;
  else
    goto __BB6;

  __BB(6):
  goto __BB7;

  __BB(7):
  res_1 = __PHI (__BB4: a_3(D), __BB6: b_4(D));
  return res_1;
}

/* We should be able to detect MAX despite the extra edge into
   the middle BB.  */
/* { dg-final { scan-tree-dump "MAX" "phiopt1" } } */
