/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-reassoc1" } */
int main(int a, int b, int c, int d)
{
  /* All the xor's cancel each other out, leaving 0  */
  int e = (a ^ b) ^ (c ^ d);
  int f = (c ^ a) ^ (b ^ d);
  return e ^ f;
}
/* { dg-final { scan-tree-dump-times "= 0" 1 "reassoc1"} } */
