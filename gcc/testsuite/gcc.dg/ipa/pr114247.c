/* { dg-do run } */
/* { dg-options "-O2 -fsigned-char -fno-strict-aliasing -fwrapv" } */

union a {
  unsigned short b;
  int c;
  signed short d;
};
int e, f = 1, g;
long h;
const int **i;
void j(union a k, int l, unsigned m) {
  const int *a[100];
  i = &a[0];
  h = k.d;
}
static int o(union a k) {
  k.d = -1;
  while (1)
    if (f)
      break;
  j(k, g, e);
  return 0;
}
int main() {
  union a n = {1};
  o(n);
  if (h != -1)
    __builtin_abort();
  return 0;
}
