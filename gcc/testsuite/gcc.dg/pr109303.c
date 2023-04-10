/* PR ipa/109303 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */

struct __attribute__((packed)) A { char c1; short a1[__INT_MAX__]; };
struct __attribute__((packed)) B { char c2; short a2[100]; };
struct S { struct A p1; struct B p2[4]; };
void bar (short int);

static void
foo (struct S *q)
{
  for (int i = 0; i < q->p1.c1; i++)
    for (int j = 0; j < q->p2[i].c2; j++)
      bar (q->p2[i].a2[j]);
}

int
main ()
{
  struct S q = {};
  q.p2[0].c2 = q.p2[1].c2 = 3;
  foo (&q);
}
