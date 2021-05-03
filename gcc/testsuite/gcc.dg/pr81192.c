/* { dg-options "-Os -fdump-tree-pre-details -fdisable-tree-evrp -fno-tree-dse" } */

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

#if __SIZEOF_INT__ == 2
#define unsigned __UINT32_TYPE__
#define int __INT32_TYPE__
#endif

unsigned a;
int b, c;

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

/* { dg-final { scan-tree-dump-times "(?n)find_duplicates: <bb .*> duplicate of <bb .*>" 1 "pre" } } */
