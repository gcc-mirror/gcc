/* { dg-do compile } */

int a, b, *c = &b;
static void g(int i) {
  int d = 0, e, f[] = {a}, j = a;
  e = b;
  if (e - i)
    return;
  a = 0;
h:
  if (e) {
    e = j;
    if (f[3])
      goto k;
    goto h;
  }
  while (1) {    d = -1;
    while (1) {
      if (d - 1 - j < 0)
        return;
    k:
      if (f[1])
        break;
    }
  }
}
int main() {
  g(1);
  return 0;
}
