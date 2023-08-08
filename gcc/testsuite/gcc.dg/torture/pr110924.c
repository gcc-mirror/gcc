/* { dg-do compile } */

int a[1], b, c, d, e, f, g;
void h(int i, int j) {
  int *k = 0;
  if (*k)
    h(0, 0);
  g = i && d;
}
int main() {
  if (c)
    goto l;
  if (!a)
    while (1) {
      f = 1;
      while (f)
        h(b && main(), e);
      while (1)
        ;
    l:;
    }
  return 0;
}
