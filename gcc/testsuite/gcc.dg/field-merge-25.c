/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-fre" } */

/* PR tree-optimization/118706 */

int a[1][1][3], b;
int main() {
  int c = -1;
  while (b) {
    if (a[c][c][6])
      break;
    if (a[0][0][0])
      break;
  }
}
