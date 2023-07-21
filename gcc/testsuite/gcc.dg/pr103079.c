/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-vrp2 -fno-tree-ch" } */

int a, b = -2;
int main() {
  int d = 0;
  int t;
  if (b)
    goto t1;
  if (t) {
t1:
    if (!a)
      d = b;
    while (d > -1)
      ;
  }
  return 0;
}
/* { dg-final { scan-tree-dump "PHI" "vrp2" } } */

