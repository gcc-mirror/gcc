/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-reassoc1" } */

int main(int a, int b, int c, int d, int e, int f, int g, int h)
{
  /* Should be transformed into e = 20 */
  int i = (a + 9) + (c + 8);
  int j = (-c + 1) + (-a + 2);

  e = i + j;
  return e;
}

/* We can always re-associate to a final constant but the current
   implementation does not allow easy roll-back without IL changes.  */

/* { dg-final { scan-tree-dump-times "= 20" 1 "reassoc1" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
