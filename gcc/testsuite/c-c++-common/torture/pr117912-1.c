/* { dg-do run } */

struct S { int a; int b[24]; int c[24]; int d; };
volatile int *p;

void __attribute__((noipa))
bar (int *q)
{
 p = q;
}

__SIZE_TYPE__ __attribute__((noipa))
foo (struct S *p)
{
  bar (&p->b[24]);
  bar (&p->c[0]);
  return __builtin_object_size (&p->c[0], 1);
}

int
main()
{
  struct S s;
  __SIZE_TYPE__ x = foo (&s);
  if (x < sizeof (int) * 24)
    __builtin_abort ();
  return 0;
}
