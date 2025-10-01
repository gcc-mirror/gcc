/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-loop-im" } */

int a, b, c;
void d(int[]);
void e(int f[][2]) {
g:
  b = f[0][1];
  if (c)
    goto h;
i:
  if (a)
    goto g;
  if (f[1][1])
    goto j;
h:
  if (f[1][1])
    goto i;
  goto k;
j:
  b--;
  if (b + f[0][1])
    goto i;
k:
  int l[] = {f[0][1]};
  d(l);
}
