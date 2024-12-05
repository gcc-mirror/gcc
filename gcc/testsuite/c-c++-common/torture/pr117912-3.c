/* { dg-do run } */
/* { dg-additional-options "-std=gnu++20" { target c++ } } */

struct B {};
struct A { int a;
#ifdef __cplusplus
	   [[no_unique_address]]
#endif
	   struct B b;
	   char c[]; };
volatile void *p;

void __attribute__((noipa))
bar (void *q)
{
  p = q;
}

__SIZE_TYPE__ __attribute__((noipa))
foo (struct A *p)
{
  bar (&p->b);
  bar (&p->c);
  return __builtin_object_size (&p->c, 1);
}

__SIZE_TYPE__ __attribute__((noipa))
baz (void)
{
  struct A *p = (struct A *) __builtin_malloc (__builtin_offsetof (struct A, c) + 64);
  bar (&p->b);
  bar (&p->c);
  return __builtin_object_size (&p->c, 1);
}

__SIZE_TYPE__ __attribute__((noipa))
qux (struct A *p)
{
  bar (&p->b);
  bar (&p->c);
  return __builtin_object_size (&p->c, 3);
}

__SIZE_TYPE__ __attribute__((noipa))
boo (void)
{
  struct A *p = (struct A *) __builtin_malloc (__builtin_offsetof (struct A, c) + 64);
  bar (&p->b);
  bar (&p->c);
  return __builtin_object_size (&p->c, 3);
}

int
main ()
{
  static struct A a = { .a = 1, .b = {}, .c = { 1, 2, 3, 4, 0 } };
  if (foo (&a) < 5)
    __builtin_abort ();
  if (baz () < 64)
    __builtin_abort ();
}
