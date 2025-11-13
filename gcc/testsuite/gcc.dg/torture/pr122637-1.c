/* { dg-do compile } */
/* PR tree-optimization/122637 */

char a[10][3];
int b;
void e(short c) {
  for (; c; c--) {
    for (int d = 2; d; d--)
      b = a[d][0] & a[c][d];
  }
}
