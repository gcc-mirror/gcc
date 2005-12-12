/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-reassoc1" } */

int main(int a, int b, int c, int d, int e, int f, int g, int h)
{
  /* e & ~e -> 0 */
  int i = (a & 9) & (c & d);
  int j = (~e & d) & (~c & e);
  e = i & j;
  return e;
}
/* { dg-final { scan-tree-dump-times "= 0" 1 "reassoc1"} } */
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
