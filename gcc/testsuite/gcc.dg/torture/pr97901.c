/* { dg-do compile } */

int a[1], b, *c, *d;

int main() {
L:
  d = c;
  for (b = 0; b < 2; b++)
    d = &a[0];
  if (c)
    goto L;
  if (*d)
    __builtin_abort ();
  return 0;
}
