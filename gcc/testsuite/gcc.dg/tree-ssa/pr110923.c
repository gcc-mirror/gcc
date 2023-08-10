/* { dg-do compile } */ 
/* { dg-options "-O3 -fdump-tree-lsplit-details-blocks" } */
int a, b, c, d;
int main() {
  for (a = 0; a < 2; a++) {
    if (b > 2)
      c = 0;
    if (b > a)
      d = 0;
  }
  return 0;
}
/* { dg-final { scan-tree-dump-times "loop split" 1 "lsplit" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "lsplit" } } */
