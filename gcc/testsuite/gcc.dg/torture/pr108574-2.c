/* { dg-do run } */

int a, b, c, d, e, f, g = -1, h;
void l() {
  if (!e)
    goto i;
  for (; g; g++) {
    b = ~d;
    int j = 0, k = 1;
    if (k && (b || f))
      j = b;
  i:
    a = ~j;
  }
}
int main() {
  h = 3;
  for (; h; h--) {
    e = 1;
    int m = ~a, n = 1 % m;
    c = n;
    l();
  }
  return 0;
}
