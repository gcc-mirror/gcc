/* { dg-do run } */
/* { dg-options "-fsanitize=bounds -Wall -Wextra -Wno-unused -Wno-array-bounds" } */

/* Test flexible array member-like arrays.  Normal flexible array members
   are tested in bounds-1.c.  Test non-strict mode.  */

__attribute__ ((noinline, noclone))
void
fn1 (void)
{
  volatile struct S { char a[1]; char b; } s;
  s.a[0] = 1; // OK
  s.a[1] = 2; // error
  volatile struct S *p = &s;
  p->a[0] = 1; // OK
  p->a[1] = 1; // error
}

__attribute__ ((noinline, noclone))
void
fn2 (void)
{
  struct S { int c; char d[4]; };
  volatile struct T { int e; struct S f; int g; } t;
  t.f.d[3] = 1; // OK
  t.f.d[4] = 1; // error
  volatile struct T *p = &t;
  p->f.d[3] = 1; // OK
  p->f.d[4] = 1; // error
}

__attribute__ ((noinline, noclone))
void
fn3 (void)
{
  volatile struct S { char b; char a[1]; } s;
  s.a[0] = 1; // OK
  s.a[1] = 1; // error
  volatile struct S *p = &s;
  p->a[0] = 1; // OK
  p->a[1] = 1; // error in strict mode
}

__attribute__ ((noinline, noclone))
void
fn4 (void)
{
  volatile struct S { char b; char a[1]; } s;
  volatile struct T { struct S s; int i; } t;
  t.s.a[0] = 1; // OK
  t.s.a[1] = 1; // error
  volatile struct T *pt = &t;
  pt->s.a[0] = 1; // OK
  pt->s.a[1] = 1; // error
}

__attribute__ ((noinline, noclone))
void
fn5 (void)
{
  volatile struct S { char b; char a[1]; } s;
  volatile struct U { int a; struct S s; } u;
  u.s.a[0] = 1; // OK
  u.s.a[1] = 1; // error
  volatile struct U *pu = &u;
  pu->s.a[0] = 1; // OK
  pu->s.a[1] = 1; // error in strict mode
}

int
main (void)
{
  fn1 ();
  fn2 ();
  fn3 ();
  fn4 ();
  fn5 ();
  return 0;
}

/* { dg-output "index 1 out of bounds for type 'char \\\[1\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 1 out of bounds for type 'char \\\[1\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 4 out of bounds for type 'char \\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 4 out of bounds for type 'char \\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 1 out of bounds for type 'char \\\[1\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 1 out of bounds for type 'char \\\[1\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 1 out of bounds for type 'char \\\[1\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 1 out of bounds for type 'char \\\[1\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
