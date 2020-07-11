/* { dg-do run } */
/* { dg-options "-fsanitize=bounds -Wall -Wextra -Wno-unused -Wno-array-bounds -Wno-uninitialized" } */
/* { dg-options "-fsanitize=bounds -Wall -Wextra -Wno-unused -Wno-array-bounds -Wno-uninitialized -Wno-volatile" { target c++ } } */

/* Test runtime errors.  */

struct S { int a[10]; };
struct T { int a[5]; int s[2]; };

int
foo_5 (void)
{
  return 5;
}

__attribute__ ((noinline, noclone))
void
fn_p (int p)
{
  (void) p;
}

static void __attribute__ ((noinline, noclone))
fn1 (void)
{
  volatile int a[5];
  asm ("" : : "r" (&a) : "memory");
  a[2] = a[5];
}

static void __attribute__ ((noinline, noclone))
fn2 (void)
{
  volatile int a[5];
  volatile int j;
  int i = 5;
  int *p = &i;
  asm ("" : : "r" (&a) : "memory");
  j = a[*p];
}

static void __attribute__ ((noinline, noclone))
fn3 (void)
{
  volatile int a[5];
  fn_p (a[5]);
}

static void __attribute__ ((noinline, noclone))
fn4 (void)
{
  struct T t;
  asm ("" : : "r" (&t.a) : "memory");
  t.a[foo_5 ()] = 1;
}

static void __attribute__ ((noinline, noclone))
fn5 (void)
{
  int i = 5;
  volatile int a[i];
  asm ("" : : "r" (&a) : "memory");
  a[2] = a[i];
}

static void __attribute__ ((noinline, noclone))
fn6 (void)
{
  int i = 5;
  volatile int a[i];
  volatile int j;
  fn_p (a[i]);
  asm ("" : : "r" (&a) : "memory");
  j = a[foo_5 ()];
}

static void __attribute__ ((noinline, noclone))
fn7 (void)
{
  int n = 5;
  volatile int i;
  volatile int c[n][n][n];
  asm ("" : : "r" (&c[5]) : "memory");
  i = c[5][2][2];
  asm ("" : : "r" (&c[2]) : "memory");
  i = c[2][5][2];
  asm ("" : : "r" (&c[2]) : "memory");
  i = c[2][2][5];
}

static void __attribute__ ((noinline, noclone))
fn8 (void)
{
  volatile int i;
  volatile struct S s;
  asm ("" : : "r" (&s.a) : "memory");
  i = s.a[10];
}

static void __attribute__ ((noinline, noclone))
fn9 (void)
{
  long int *volatile d[10][5];
  asm ("" : : "r" (&d[10]) : "memory");
  d[8][3] = d[10][0];
}

static void __attribute__ ((noinline, noclone))
fn10 (void)
{
  /* Beware of side-effects.  */
  volatile int x = 10;
  volatile int e[20];
  e[x++] = 3;
  if (x != 11)
    __builtin_abort ();
  e[x--] = 3;
  if (x != 10)
    __builtin_abort ();
}

static void __attribute__ ((noinline, noclone))
fn11 (void)
{
  char ***volatile f[5];
  asm ("" : : "r" (&f) : "memory");
  f[2] = f[5];
}

static void __attribute__ ((noinline, noclone))
fn12 (int i)
{
  volatile int a[5] = { };
  int k = i ? a[i] : i;
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
  fn12 (5);
  return 0;
}

/* { dg-output "index 5 out of bounds for type 'int \\\[5\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 5 out of bounds for type 'int \\\[5\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 5 out of bounds for type 'int \\\[5\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 5 out of bounds for type 'int \\\[5\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 5 out of bounds for type 'int \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 5 out of bounds for type 'int \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 5 out of bounds for type 'int \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 5 out of bounds for type 'int \\\[\\\*\\\]\\\[\\\*\\\]\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 5 out of bounds for type 'int \\\[\\\*\\\]\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 5 out of bounds for type 'int \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 10 out of bounds for type 'int \\\[10\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 10 out of bounds for type 'long int \\\*\\\[10\\\]\\\[5\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 5 out of bounds for type 'char \\\*\\\*\\\*\\\[5\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 5 out of bounds for type 'int \\\[5\\\]'" } */
