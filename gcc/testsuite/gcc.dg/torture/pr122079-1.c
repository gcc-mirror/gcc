/* { dg-do compile } */
/* { dg-additional-options "-fcode-hoisting" } */

int a, b, c;
void e(int *f) {
  int d = 0;
  if (f)
    goto g;
  goto h;
i:
  d = 1 + f[0];
j:
  if (c)
    goto h;
k:
  if (b)
    goto i;
  if (a)
    goto j;
g:
  if (d + f[0])
    goto k;
h:
  int l[] = {f[0]};
  if (a)
    e(l);
}
