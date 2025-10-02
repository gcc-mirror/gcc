/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-ccp -fdump-tree-evrp" } */
int main() {
  int a = -1, b = 2, c = 1;
  if (a >= 0)
    c = 0;
  while (1) {
    if (-b + c - 7 >= 0)
      return 0;
    b = b - 1000 - 2147482648;
  }
}
/* { dg-final { scan-tree-dump "return 0"  "evrp" } } */
