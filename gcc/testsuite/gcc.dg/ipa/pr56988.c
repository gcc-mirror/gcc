/* { dg-do run } */
/* { dg-options "-O3" } */
/* { dg-add-options bind_pic_locally } */

struct S
{
  int a, b, c;
};

volatile int g;

static void __attribute__ ((noinline, noclone))
bar (struct S **p)
{
  g = 5;
};

static void __attribute__ ((noinline))
foo (struct S *p)
{
  int i = p->a;
  if (i != 1)
    __builtin_abort ();
  bar (&p);
}

int
main (int argc, char *argv[])
{
  struct S s;
  s.a = 1;
  s.b = 64;
  s.c = 32;
  foo (&s);

  return 0;
}

