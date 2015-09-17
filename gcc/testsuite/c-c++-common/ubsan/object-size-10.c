/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=undefined" } */

static char a[128] __attribute__ ((aligned(4096)));
static int b[128] __attribute__ ((aligned(4096)));

__attribute__ ((noinline, noclone)) int
fn1 (int i)
{
  asm ("");
  return a[i & 127];
}

__attribute__ ((noinline, noclone)) int
fn2 (int i)
{
  asm ("");
  return a[i & 128];
}

/* { dg-output "index 128 out of bounds for type 'char \\\[128\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */

__attribute__ ((noinline, noclone)) int
fn3 (int i)
{
  asm ("");
  return b[i & 127];
}

__attribute__ ((noinline, noclone)) int
fn4 (int i)
{
  asm ("");
  return b[i & 128];
}

/* { dg-output "\[^\n\r]*index 128 out of bounds for type 'int \\\[128\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */

__attribute__ ((noinline, noclone)) int
fn5 (int i, int j)
{
  asm ("");
  return b[i & j];
}

/* { dg-output "\[^\n\r]*index 128 out of bounds for type 'int \\\[128\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^" } */

__attribute__ ((noinline, noclone)) int
fn6 (int i)
{
  asm ("");
  return b[i & 0];
}

int
main (void)
{
  fn1 (128);
  fn2 (128);
  fn3 (128);
  fn4 (128);
  fn5 (128, 127);
  fn5 (128, 128);
  fn6 (128);
  return 0;
}
