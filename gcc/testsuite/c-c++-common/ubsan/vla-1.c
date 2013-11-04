/* { dg-do run } */
/* { dg-options "-fsanitize=vla-bound -w" } */

static int
bar (void)
{
  return -42;
}

typedef long int V;
int
main (void)
{
  int x = -1;
  double di = -3.2;
  V v = -666;

  int a[x];
  int aa[x][x];
  int aaa[x][x][x];
  int b[x - 4];
  int c[(int) di];
  int d[1 + x];
  int e[1 ? x : -1];
  int f[++x];
  int g[(signed char) --x];
  int h[(++x, --x, x)];
  int i[v];
  int j[bar ()];

  return 0;
}

/* { dg-output "variable length array bound evaluates to non-positive value -1(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -1(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -1(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -1(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -1(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -1(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -5(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -3(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value 0(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -1(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value 0(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -1(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -1(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -666(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -42(\n|\r\n|\r)" } */
