/* { dg-options "-Os -fgimple -fdump-tree-pre-details -fdisable-tree-evrp -fno-tree-dse" } */

#if __SIZEOF_INT__ == 2
#define unsigned __UINT32_TYPE__
#define int __INT32_TYPE__
#endif

unsigned a;
int b, c;

void __GIMPLE(ssa, startwith("pre")) fn2   ()
{
  int b_lsm6;
  int j;
  int c0_1;
  int iftmp2_8;

  __BB(2):
  a = _Literal (unsigned)30;
  c0_1 = c;
  b_lsm6_9 = b;
  goto __BB7;

  __BB(3):
  if (j_6(D) != _Literal (int)2147483647)
    goto __BB4;
  else
    goto __BB9;

  __BB(4):
  iftmp2_8 = j_6(D) + _Literal (int)1;
  goto __BB5;

  __BB(9):
  iftmp2_8 = j_6(D) + _Literal (int)1;
  goto __BB5;

  __BB(5):
  b_lsm6_10 = _Literal (int)2147483647;
  goto __BB6;

  __BB(6):
  if (c0_1 != _Literal (int) 0)
    goto __BB3;
  else
    goto __BB8;

  __BB(8):
  goto __BB7;

  __BB(7):
  goto __BB6;

}

#if 0
/* This used to be a C based testcase but ccp3 would now would remove
   the setting of iftmp2_8 (in the above gimple) which would cause PRE
   not to test what PRE was doing incorrectly. The original code is below. */
/* Disable tree-evrp because the new version of evrp sees
<bb 3> :
  if (j_8(D) != 2147483647)
    goto <bb 4>; [50.00%]
  else
    goto <bb 5>; [50.00%]
<bb 4> :
  iftmp.2_11 = j_8(D) + 1;
<bb 5> :
  # iftmp.2_12 = PHI <j_8(D)(3), iftmp.2_11(4)>

EVRP now recognizes a constant can be propagated into the 3->5 edge and
produces
  # iftmp.2_12 = PHI <2147483647(3), iftmp.2_11(4)> 
which causes the situation being tested to dissapear before we get to PRE.  */

/* Likewise disable DSE which also elides the tail merging "opportunity".  */

static int
fn1 (int p1, int p2)
{
  return p1 > 2147483647 - p2 ? p1 : p1 + p2;
}

void
fn2 (void)
{
  int j;
  a = 30;
  for (; a;)
    for (; c; b = fn1 (j, 1))
      ;
}
#endif

/* { dg-final { scan-tree-dump-times "(?n)find_duplicates: <bb .*> duplicate of <bb .*>" 1 "pre" } } */
