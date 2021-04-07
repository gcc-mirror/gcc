/* PR tree-optimization/98736 */

int a[6];
char b, c;
int main() {
  int d[4] = {0, 0, 0, 0};
  for (c = 0; c <= 5; c++) {
    for (b = 2; b != 0; b++)
      a[c] = 8;
    a[c] = d[3];
  }
  if (a[0] != 0)
    __builtin_abort();
}
