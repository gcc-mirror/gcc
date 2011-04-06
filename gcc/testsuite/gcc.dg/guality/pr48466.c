/* PR debug/48466 */
/* { dg-do run } */
/* { dg-options "-g" } */
/* { dg-skip-if "" { *-*-* }  { "*" } { "-O0" } } */

struct S { unsigned int a; unsigned int *b; };
struct T { struct S a; struct S b; };
struct U { const char *u; };
int n[10];
volatile int v;

struct U
foo (const char *s)
{
  struct U r;
  r.u = s;
  return r;
}

void
bar (struct T *s, int a, int b)
{
  s->a.a = a;
  s->a.b = &s->a.a;
  s->b.a = b;
  s->b.b = &s->b.a;
}

int
main ()
{
  struct T t;
  struct U x = foo ("this is x");
  struct S y, z;
  y.b = n;		/* { dg-final { gdb-test 38 "t.a.a" "17" } } */
  y.a = 0;		/* { dg-final { gdb-test 38 "*t.a.b" "17" } } */
  bar (&t, 17, 21);	/* { dg-final { gdb-test 38 "t.b.a" "21" } } */
  v++;			/* { dg-final { gdb-test 38 "*t.b.b" "21" } } */
  z = y;
  return 0;
}
