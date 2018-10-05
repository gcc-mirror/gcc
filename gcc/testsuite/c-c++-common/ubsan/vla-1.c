/* { dg-do run } */
/* { dg-options "-fsanitize=vla-bound -Wall -Wno-unused-variable -fno-stack-clash-protection" } */

typedef long int V;
int x = -1;
double di = -3.2;
V v = -6;

static int __attribute__ ((noinline, noclone))
bar (void)
{
  return -4;
}

static void __attribute__ ((noinline, noclone))
fn1 (void)
{
  int a[x];
}

static void __attribute__ ((noinline, noclone))
fn2 (void)
{
  int a[x][x];
}

static void __attribute__ ((noinline, noclone))
fn3 (void)
{
  int a[x][x][x];
}

static void __attribute__ ((noinline, noclone))
fn4 (void)
{
  int b[x - 4];
}

static void __attribute__ ((noinline, noclone))
fn5 (void)
{
  int c[(int) di];
}

static void __attribute__ ((noinline, noclone))
fn6 (void)
{
  int d[1 + x];
}

static void __attribute__ ((noinline, noclone))
fn7 (void)
{
  int e[1 ? x : -1];
}

static void __attribute__ ((noinline, noclone))
fn8 (void)
{
  int f[++x];
}

static void __attribute__ ((noinline, noclone))
fn9 (void)
{
  int g[(signed char) --x];
}

static void __attribute__ ((noinline, noclone))
fn10 (void)
{
  int h[(++x, --x, x)];
}

static void __attribute__ ((noinline, noclone))
fn11 (void)
{
  int i[v];
}

static void __attribute__ ((noinline, noclone))
fn12 (void)
{
  int j[bar ()];
}

int
main (void)
{
  fn1 ();
  fn2 ();
  fn3 ();
  fn4 ();
  fn5 ();
  fn6 ();
  fn7 ();
  fn8 ();
  fn9 ();
  fn10 ();
  fn11 ();
  fn12 ();
  return 0;
}

/* { dg-output "variable length array bound evaluates to non-positive value -1\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -1\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -1\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -1\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -1\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -1\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -5\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -3\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value 0\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -1\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value 0\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -1\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -1\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -6\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*variable length array bound evaluates to non-positive value -4" } */
