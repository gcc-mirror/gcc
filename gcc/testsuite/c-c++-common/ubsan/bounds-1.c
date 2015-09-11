/* { dg-do run } */
/* { dg-options "-fsanitize=bounds -fno-sanitize-recover=bounds -Wall" } */

/* Don't fail on valid uses.  */

struct S { int a[10]; };
struct T { int l; int a[]; };
struct U { int l; int a[0]; };
struct V { int l; int a[1]; };

__attribute__ ((noinline, noclone))
void
fn_p (int p)
{
}

__attribute__ ((noinline, noclone))
void
fn_a (volatile int a[])
{
  /* This is not instrumented.  */
  a[4] = 5;
}

__attribute__ ((noinline, noclone))
int
foo_i (int i)
{
  int a[5] = { };
  int k = i ? a[i] : i;
  return k;
}

int
main (void)
{
  volatile int a[5];
  a[4] = 1;
  a[2] = a[3];
  fn_p (a[4]);
  fn_a (a);

  int i = 4;
  a[i] = 1;
  a[2] = a[i];
  fn_p (a[i]);
  foo_i (i);

  const int n = 5;
  volatile int b[n];
  b[4] = 1;
  b[2] = b[3];
  fn_p (b[4]);
  fn_a (b);

  volatile int c[n][n][n];
  c[2][2][2] = 2;
  i = c[4][4][4];

  volatile struct S s;
  s.a[9] = 1;
  i = s.a[9];

  /* Don't instrument flexible array members.  */
  struct T *t = (struct T *) __builtin_malloc (sizeof (struct T) + 10);
  t->a[1] = 1;

  /* Don't instrument zero-sized arrays (GNU extension).  */
  struct U *u = (struct U *) __builtin_malloc (sizeof (struct U) + 10);
  u->a[1] = 1;

  /* Don't instrument last array in a struct.  */
  struct V *v = (struct V *) __builtin_malloc (sizeof (struct V) + 10);
  v->a[1] = 1;

  long int *d[10][5];
  d[9][0] = (long int *) 0;
  d[8][3] = d[9][0];

  return 0;
}
