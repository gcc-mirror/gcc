// { dg-do run }
// { dg-options "-O2" }

#include "../../gcc.dg/builtin-object-size-common.h"

typedef struct A
{
  char a[10];
  int b;
  char c[10];
  static int d;
} AT;

int A::d = 6;

void
__attribute__ ((noinline))
test1 (A *p)
{
  char *c;
  if (__builtin_object_size (&p->a, 0) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&p->a[0], 0) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&p->a[3], 0) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&p->b, 0) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&p->c, 0) != (size_t) -1)
    FAIL ();
  c = p->a;
  if (__builtin_object_size (c, 0) != (size_t) -1)
    FAIL ();
  c = &p->a[0];
  if (__builtin_object_size (c, 0) != (size_t) -1)
    FAIL ();
  c = &p->a[3];
  if (__builtin_object_size (c, 0) != (size_t) -1)
    FAIL ();
  c = (char *) &p->b;
  if (__builtin_object_size (c, 0) != (size_t) -1)
    FAIL ();
  c = (char *) &p->c;
  if (__builtin_object_size (c, 0) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&p->a, 1) != sizeof (p->a))
    FAIL ();
  if (__builtin_object_size (&p->a[0], 1) != sizeof (p->a))
    FAIL ();
  if (__builtin_object_size (&p->a[3], 1) != sizeof (p->a) - 3)
    FAIL ();
  if (__builtin_object_size (&p->b, 1) != sizeof (p->b))
    FAIL ();
  if (__builtin_object_size (&p->c, 1) != (size_t) -1)
    FAIL ();
  c = p->a;
  if (__builtin_object_size (c, 1) != sizeof (p->a))
    FAIL ();
  c = &p->a[0];
  if (__builtin_object_size (c, 1) != sizeof (p->a))
    FAIL ();
  c = &p->a[3];
  if (__builtin_object_size (c, 1) != sizeof (p->a) - 3)
    FAIL ();
  c = (char *) &p->b;
  if (__builtin_object_size (c, 1) != sizeof (p->b))
    FAIL ();
  c = (char *) &p->c;
  if (__builtin_object_size (c, 1) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&p->a, 2) != 0)
    FAIL ();
  if (__builtin_object_size (&p->a[0], 2) != 0)
    FAIL ();
  if (__builtin_object_size (&p->a[3], 2) != 0)
    FAIL ();
  if (__builtin_object_size (&p->b, 2) != 0)
    FAIL ();
  if (__builtin_object_size (&p->c, 2) != 0)
    FAIL ();
  c = p->a;
  if (__builtin_object_size (c, 2) != 0)
    FAIL ();
  c = &p->a[0];
  if (__builtin_object_size (c, 2) != 0)
    FAIL ();
  c = &p->a[3];
  if (__builtin_object_size (c, 2) != 0)
    FAIL ();
  c = (char *) &p->b;
  if (__builtin_object_size (c, 2) != 0)
    FAIL ();
  c = (char *) &p->c;
  if (__builtin_object_size (c, 2) != 0)
    FAIL ();
  if (__builtin_object_size (&p->a, 3) != sizeof (p->a))
    FAIL ();
  if (__builtin_object_size (&p->a[0], 3) != sizeof (p->a))
    FAIL ();
  if (__builtin_object_size (&p->a[3], 3) != sizeof (p->a) - 3)
    FAIL ();
  if (__builtin_object_size (&p->b, 3) != sizeof (p->b))
    FAIL ();
  if (__builtin_object_size (&p->c, 3) != 0)
    FAIL ();
  c = p->a;
  if (__builtin_object_size (c, 3) != sizeof (p->a))
    FAIL ();
  c = &p->a[0];
  if (__builtin_object_size (c, 3) != sizeof (p->a))
    FAIL ();
  c = &p->a[3];
  if (__builtin_object_size (c, 3) != sizeof (p->a) - 3)
    FAIL ();
  c = (char *) &p->b;
  if (__builtin_object_size (c, 3) != sizeof (p->b))
    FAIL ();
  c = (char *) &p->c;
  if (__builtin_object_size (c, 3) != 0)
    FAIL ();
}

void
__attribute__ ((noinline))
test2 (void)
{
  char *c;
  size_t s = 2 * sizeof (A);
  A *p = (A *) malloc (2 * sizeof (A));
  if (__builtin_object_size (&p->a, 0) != s)
    FAIL ();
  if (__builtin_object_size (&p->a[0], 0) != s)
    FAIL ();
  if (__builtin_object_size (&p->a[3], 0) != s - 3)
    FAIL ();
  if (__builtin_object_size (&p->b, 0) != s - __builtin_offsetof (A, b))
    FAIL ();
  if (__builtin_object_size (&p->c, 0) != s - __builtin_offsetof (A, c))
    FAIL ();
  c = p->a;
  if (__builtin_object_size (c, 0) != s)
    FAIL ();
  c = &p->a[0];
  if (__builtin_object_size (c, 0) != s)
    FAIL ();
  c = &p->a[3];
  if (__builtin_object_size (c, 0) != s - 3)
    FAIL ();
  c = (char *) &p->b;
  if (__builtin_object_size (c, 0) != s - __builtin_offsetof (A, b))
    FAIL ();
  c = (char *) &p->c;
  if (__builtin_object_size (c, 0) != s - __builtin_offsetof (A, c))
    FAIL ();
  if (__builtin_object_size (&p->a, 1) != sizeof (p->a))
    FAIL ();
  if (__builtin_object_size (&p->a[0], 1) != sizeof (p->a))
    FAIL ();
  if (__builtin_object_size (&p->a[3], 1) != sizeof (p->a) - 3)
    FAIL ();
  if (__builtin_object_size (&p->b, 1) != sizeof (p->b))
    FAIL ();
  if (__builtin_object_size (&p->c, 1) != s - __builtin_offsetof (A, c))
    FAIL ();
  c = p->a;
  if (__builtin_object_size (c, 1) != sizeof (p->a))
    FAIL ();
  c = &p->a[0];
  if (__builtin_object_size (c, 1) != sizeof (p->a))
    FAIL ();
  c = &p->a[3];
  if (__builtin_object_size (c, 1) != sizeof (p->a) - 3)
    FAIL ();
  c = (char *) &p->b;
  if (__builtin_object_size (c, 1) != sizeof (p->b))
    FAIL ();
  c = (char *) &p->c;
  if (__builtin_object_size (c, 1) != s - __builtin_offsetof (A, c))
    FAIL ();
  if (__builtin_object_size (&p->a, 2) != s)
    FAIL ();
  if (__builtin_object_size (&p->a[0], 2) != s)
    FAIL ();
  if (__builtin_object_size (&p->a[3], 2) != s - 3)
    FAIL ();
  if (__builtin_object_size (&p->b, 2) != s - __builtin_offsetof (A, b))
    FAIL ();
  if (__builtin_object_size (&p->c, 2) != s - __builtin_offsetof (A, c))
    FAIL ();
  c = p->a;
  if (__builtin_object_size (c, 2) != s)
    FAIL ();
  c = &p->a[0];
  if (__builtin_object_size (c, 2) != s)
    FAIL ();
  c = &p->a[3];
  if (__builtin_object_size (c, 2) != s - 3)
    FAIL ();
  c = (char *) &p->b;
  if (__builtin_object_size (c, 2) != s - __builtin_offsetof (A, b))
    FAIL ();
  c = (char *) &p->c;
  if (__builtin_object_size (c, 2) != s - __builtin_offsetof (A, c))
    FAIL ();
  if (__builtin_object_size (&p->a, 3) != sizeof (p->a))
    FAIL ();
  if (__builtin_object_size (&p->a[0], 3) != sizeof (p->a))
    FAIL ();
  if (__builtin_object_size (&p->a[3], 3) != sizeof (p->a) - 3)
    FAIL ();
  if (__builtin_object_size (&p->b, 3) != sizeof (p->b))
    FAIL ();
  if (__builtin_object_size (&p->c, 3) != s - __builtin_offsetof (A, c))
    FAIL ();
  c = p->a;
  if (__builtin_object_size (c, 3) != sizeof (p->a))
    FAIL ();
  c = &p->a[0];
  if (__builtin_object_size (c, 3) != sizeof (p->a))
    FAIL ();
  c = &p->a[3];
  if (__builtin_object_size (c, 3) != sizeof (p->a) - 3)
    FAIL ();
  c = (char *) &p->b;
  if (__builtin_object_size (c, 3) != sizeof (p->b))
    FAIL ();
  c = (char *) &p->c;
  if (__builtin_object_size (c, 3) != s - __builtin_offsetof (A, c))
    FAIL ();
  free (p);
}

void
__attribute__ ((noinline))
test3 (void)
{
  char *c;
  size_t s;
  A *p = (A *) malloc (4);
  if (__builtin_object_size (&p->a, 0) != 4)
    FAIL ();
  if (__builtin_object_size (&p->a[0], 0) != 4)
    FAIL ();
  if (__builtin_object_size (&p->a[3], 0) != 1)
    FAIL ();
  if (__builtin_object_size (&p->b, 0) != 0)
    FAIL ();
  if (__builtin_object_size (&p->c, 0) != 0)
    FAIL ();
  if (__builtin_object_size (&p->a, 1) != 4)
    FAIL ();
  if (__builtin_object_size (&p->a[0], 1) != 4)
    FAIL ();
  if (__builtin_object_size (&p->a[3], 1) != 1)
    FAIL ();
  if (__builtin_object_size (&p->b, 1) != 0)
    FAIL ();
  if (__builtin_object_size (&p->c, 1) != 0)
    FAIL ();
  free (p);
  s = __builtin_offsetof (A, c) + 4;
  p = (A *) malloc (s);
  if (__builtin_object_size (&p->a, 0) != s)
    FAIL ();
  if (__builtin_object_size (&p->a[0], 0) != s)
    FAIL ();
  if (__builtin_object_size (&p->a[3], 0) != s - 3)
    FAIL ();
  if (__builtin_object_size (&p->b, 0) != s - __builtin_offsetof (A, b))
    FAIL ();
  if (__builtin_object_size (&p->c, 0) != 4)
    FAIL ();
  if (__builtin_object_size (&p->a, 1) != sizeof (p->a))
    FAIL ();
  if (__builtin_object_size (&p->a[0], 1) != sizeof (p->a))
    FAIL ();
  if (__builtin_object_size (&p->a[3], 1) != sizeof (p->a) - 3)
    FAIL ();
  if (__builtin_object_size (&p->b, 1) != sizeof (p->b))
    FAIL ();
  if (__builtin_object_size (&p->c, 1) != 4)
    FAIL ();
  free (p);
}

struct B
{
  A a[4];
};

void
__attribute__ ((noinline))
test4 (struct B *q, int i)
{
  if (__builtin_object_size (&q->a[2].a[2], 1) != sizeof (q->a[0].a) - 2)
    FAIL ();
  if (__builtin_object_size (&q->a[2].c[2], 1) != sizeof (q->a[0].c) - 2)
    FAIL ();
  if (__builtin_object_size (&q->a[3].a[2], 1) != sizeof (q->a[0].a) - 2)
    FAIL ();
  if (__builtin_object_size (&q->a[3].c[2], 1) != sizeof (q->a[0].c) - 2)
    FAIL ();
  if (__builtin_object_size (&q->a[i].a[2], 1) != sizeof (q->a[0].a) - 2)
    FAIL ();
  if (__builtin_object_size (&q->a[i].c[2], 1) != sizeof (q->a[0].c) - 2)
    FAIL ();
}

struct C
{
  char a[10];
  char b;
};

void
__attribute__ ((noinline))
test5 (struct C *c)
{
  if (__builtin_object_size (&c->b, 0) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&c->b, 1) != 1)
    FAIL ();
  if (__builtin_object_size (&c->b, 2) != 0)
    FAIL ();
  if (__builtin_object_size (&c->b, 3) != 1)
    FAIL ();
}

struct D
{
  int i;
  struct D1
  {
    char b;
    char a[10];
  } j;
};

void
__attribute__ ((noinline))
test6 (struct D *d)
{
  if (__builtin_object_size (&d->j.a[3], 0) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&d->j.a[3], 1) != sizeof (d->j.a) - 3)
    FAIL ();
  if (__builtin_object_size (&d->j.a[3], 2) != 0)
    FAIL ();
  if (__builtin_object_size (&d->j.a[3], 3) != sizeof (d->j.a) - 3)
    FAIL ();
}

struct E
{
  int i;
  struct E1
  {
    char b;
    char a[10];
  } j[1];
};

void
__attribute__ ((noinline))
test7 (struct E *e)
{
  if (__builtin_object_size (&e->j[0].a[3], 0) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&e->j[0].a[3], 1) != sizeof (e->j[0].a) - 3)
    FAIL ();
  if (__builtin_object_size (&e->j[0].a[3], 2) != 0)
    FAIL ();
  if (__builtin_object_size (&e->j[0].a[3], 3) != sizeof (e->j[0].a) - 3)
    FAIL ();
  if (__builtin_object_size ((char *) &e->j[0], 0) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size ((char *) &e->j[0], 1) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size ((char *) &e->j[0], 2) != 0)
    FAIL ();
  if (__builtin_object_size ((char *) &e->j[0], 3) != 0)
    FAIL ();
}

union F
{
  char a[1];
  struct F1
  {
    char b;
    char c[10];
  } d;
};

void
__attribute__ ((noinline))
test8 (union F *f)
{
  if (__builtin_object_size (&f->d.c[3], 0) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&f->d.c[3], 1) != (size_t) -1)
    FAIL ();
  if (__builtin_object_size (&f->d.c[3], 2) != 0)
    FAIL ();
  if (__builtin_object_size (&f->d.c[3], 3) != 0)
    FAIL ();
}

int
main (void)
{
  A a, *p = &a;
  int i = 1;
  __asm ("" : "+r" (p));
  test1 (p);
  test2 ();
  test3 ();
  struct B b, *q = &b;
  __asm ("" : "+r" (q), "+r" (i));
  test4 (q, i);
  struct C c, *cp = &c;
  __asm ("" : "+r" (cp));
  test5 (cp);
  struct D d, *dp = &d;
  __asm ("" : "+r" (dp));
  test6 (dp);
  struct E e, *ep = &e;
  __asm ("" : "+r" (ep));
  test7 (ep);
  union F f, *fp = &f;
  __asm ("" : "+r" (fp));
  test8 (fp);
  DONE ();
}
