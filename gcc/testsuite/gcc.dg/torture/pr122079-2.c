/* { dg-do compile } */

int a, b, *c = &a, d, e, f;
void g(int *p) { a = p[0]; }
int main() {
  int h = 0;
i:
  d = c[0];
  c[0] = h;
  if (a)
    goto j;
k:
  h = c[0] - 1;
  while (1) {
    if (b)
      goto i;
    if (f)
      goto k;
  j:
    if (!e) {
      int m[] = {c[0]};
      g(m);
      break;
    }
  }
  return 0;
}
