/* { dg-do run } */
/* { dg-options "-fsanitize=bounds" } */

/* Test negative bounds.  */

struct S { int a[10]; };

__attribute__ ((noinline, noclone))
void
fn1 (void)
{
  volatile int i;
  int m = -1;
  volatile int a[7];
  asm ("" : : "r" (&a) : "memory");
  i = a[-1];
  asm ("" : : "r" (&a) : "memory");
  i = a[m];
}

__attribute__ ((noinline, noclone))
void
fn2 (void)
{
  volatile int i;
  int m = 7;
  volatile int a[m];
  asm ("" : : "r" (&a) : "memory");
  i = a[-1];
}

__attribute__ ((noinline, noclone))
void
fn3 (void)
{
  volatile int i;
  volatile struct S s;
  asm ("" : : "r" (&s.a) : "memory");
  i = s.a[-1];
}

int
main (void)
{
  fn1 ();
  fn2 ();
  fn3 ();
  return 0;
}

/* { dg-output "index -1 out of bounds for type 'int \\\[7\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index -1 out of bounds for type 'int \\\[7\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index -1 out of bounds for type 'int \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index -1 out of bounds for type 'int \\\[10\\\]'" } */
