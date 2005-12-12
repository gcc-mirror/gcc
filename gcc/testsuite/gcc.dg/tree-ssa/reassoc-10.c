/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-optimized" } */
int main(int a, int b, int c, int d)
{
  /* Should become just a & b & c & d */
  int e = (a & b) & (c & d);
  int f = (c & a) & (b & d);
  return e & f;
}
/* { dg-final { scan-tree-dump-times "\\\& " 3 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
