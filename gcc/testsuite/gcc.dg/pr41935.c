/* PR middle-end/41935 */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);
struct A { int a; int b[10]; };

int
foo (struct A *p)
{
  return __builtin_offsetof (struct A, b[p->a]);
}

int
main ()
{
  struct A a;
  a.a = 7;
  if (foo (&a) != 7 * sizeof (int) + __builtin_offsetof (struct A, b))
    abort ();
  a.a = 2;
  if (foo (&a) != 2 * sizeof (int) + __builtin_offsetof (struct A, b))
    abort ();
  return 0;
}
