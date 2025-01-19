/* Test GNU C11 support for empty initializers.  */
/* { dg-do run } */
/* { dg-options "-std=gnu11" } */

extern void abort (void);
extern void *memset (void *, int, __SIZE_TYPE__);
#define offsetof(TYPE, MEMBER) __builtin_offsetof (TYPE, MEMBER)

struct A { unsigned char a; long long b; };
struct B { unsigned char a; long long b; struct A c[3]; };
struct C { struct A a; };
struct D { unsigned char a; long long b; struct C c; };
union U { unsigned char a; long long b; };

__attribute__((noipa)) void
check_A_padding (struct A *p)
{
  unsigned char *q = (unsigned char *) p;
  unsigned char *r = (unsigned char *) p;
  for (q += offsetof (struct A, a) + 1; q != r + offsetof (struct A, b); ++q)
    if (*q != 0)
      abort ();
}

__attribute__((noipa)) void
check_B_padding (struct B *p)
{
  unsigned char *q = (unsigned char *) p;
  unsigned char *r = (unsigned char *) p;
  for (q += offsetof (struct B, a) + 1; q != r + offsetof (struct B, b); ++q)
    if (*q != 0)
      abort ();
  for (int i = 0; i < 3; ++i)
    check_A_padding (&p->c[i]);
}

__attribute__((noipa)) void
check_D_padding (struct D *p)
{
  unsigned char *q = (unsigned char *) p;
  unsigned char *r = (unsigned char *) p;
  for (q += offsetof (struct D, a) + 1; q != r + offsetof (struct D, b); ++q)
    if (*q != 0)
      abort ();
  check_A_padding (&p->c.a);
}

__attribute__((noipa)) void
check_U_padding (union U *p)
{
  unsigned char *q = (unsigned char *) p;
  unsigned char *r = (unsigned char *) p;
  for (q += 1; q != r + sizeof (union U); ++q)
    if (*q != 0)
      abort ();
}

__attribute__((noipa)) void
check (struct A *a, struct B *b, struct B *c, struct B *d, struct B *e,
       struct B *f, struct B *g, union U *h, union U *i, union U *j,
       union U *k, struct D *l, struct D *m, struct D *n)
{
  /* Empty initializer in GNU C11 doesn't guarantee anything about
     padding bits in the initializer directly, but padding in omitted members
     is guaranteed to be zero initialized since C11.  */
  if (a->a != 0 || a->b != 0)
    abort ();
  if (b->a != 0 || b->b != 0)
    abort ();
  for (int i = 0; i < 3; ++i)
    if (b->c[i].a != 0 || b->c[i].b != 0)
      abort ();
    else
      check_A_padding (&b->c[i]);
  /* In *c the padding between c->a and c->b is indeterminate, but
     padding in c->c[0] (and 1 and 2) zero initialized (already since C11).  */
  if (c->a != 1 || c->b != 2)
    abort ();
  for (int i = 0; i < 3; ++i)
    if (c->c[i].a != 0 || c->c[i].b != 0)
      abort ();
    else
      check_A_padding (&c->c[i]);
  /* In *d the padding between d->a and d->b is indeterminate, but
     padding in d->c[0] (and 1) zero initialized (already since C11),
     padding in d->c[2] again indeterminate.  */
  if (d->a != 2 || d->b != 1)
    abort ();
  for (int i = 0; i < 2; ++i)
    if (d->c[i].a != 0 || d->c[i].b != 0)
      abort ();
    else
      check_A_padding (&d->c[i]);
  if (d->c[2].a != 3 || d->c[2].b != 4)
    abort ();
  /* In *e all padding is indeterminate.  */
  if (e->a != 1 || e->b != 2)
    abort ();
  for (int i = 0; i < 3; ++i)
    if (e->c[i].a != 3 + 2 * i || e->c[i].b != 4 + 2 * i)
      abort ();
  /* In *f likewise.  */
  if (f->a != 1 || f->b != 2)
    abort ();
  for (int i = 0; i < 3; ++i)
    if (f->c[i].a != 3 + 2 * i || f->c[i].b != 4 + 2 * i)
      abort ();
  /* In *g all padding is indeterminate.  */
  if (g->a != 1 || g->b != 2)
    abort ();
  for (int i = 0; i < 3; ++i)
    if (g->c[i].a != 3 + 2 * i || g->c[i].b != 4 + 2 * i)
      abort ();
  /* In *h h->a is default initialized and padding indeterminate.  */
  if (h->a != 0)
    abort ();
  /* In *i (and *j) i->a is initialized and padding indeterminate.  */
  if (i->a != 1 || j->a != 1)
    abort ();
  /* In *k k->b is initialized and there is (likely) no padding.  */
  if (k->b != 1)
    abort ();
  /* Padding in omitted members is zero initialized since C11.  */
  if (l->a != 0 || l->b != 0 || l->c.a.a != 0 || l->c.a.b != 0)
    abort ();
  check_A_padding (&l->c.a);
  /* In *m the padding between m->a and m->b is indeterminate, but
     padding in m->c.a is zero initialized (already since C11).  */
  if (m->a != 1 || m->b != 2 || m->c.a.a != 0 || m->c.a.b != 0)
    abort ();
  check_A_padding (&m->c.a);
  /* In *n the padding between n->a and n->b is indeterminate,
     and padding in n->c.a too.  */
  if (n->a != 1 || n->b != 2 || n->c.a.a != 3 || n->c.a.b != 4)
    abort ();
}

__attribute__((noipa)) void
test (void)
{
  struct A a = {};
  struct B b = {};
  struct B c = { 1, 2 };
  struct B d = { .b = 1, .a = 2, .c[2].a = 3, .c[2].b = 4 };
  struct B e = { 1, 2, .c[2] = {}, .c[1] = { 9 }, .c[0] = {},
		 .c[0].a = 3, .c[0].b = 4, .c[1].a = 5, .c[1].b = 6,
		 .c[2].a = 7, .c[2].b = 8 };
  struct B f = { 1, 2, {},
		 .c[0].a = 3, .c[0].b = 4, .c[1].a = 5, .c[1].b = 6,
		 .c[2].a = 7, .c[2].b = 8 };
  struct B g = { 1, 2, .c[0].a = 3, .c[0].b = 4, .c[1].a = 5, .c[1].b = 6,
		 .c[2].a = 7, .c[2].b = 8 };
  union U h = {};
  union U i = { 1 };
  union U j = { .a = 1 };
  union U k = { .b = 1 };
  struct D l = {};
  struct D m = { 1, 2 };
  struct D n = { 1, 2, {}, .c.a.a = 3, .c.a.b = 4 };
  check (&a, &b, &c, &d, &e, &f, &g, &h, &i, &j, &k, &l, &m, &n);
}

__attribute__((noipa)) void
set (struct A *a, struct B *b, struct B *c, struct B *d, struct B *e,
     struct B *f, struct B *g, union U *h, union U *i, union U *j,
     union U *k, struct D *l, struct D *m, struct D *n)
{
  memset (a, ~0, sizeof (*a));
  memset (b, ~0, sizeof (*b));
  memset (c, ~0, sizeof (*c));
  memset (d, ~0, sizeof (*d));
  memset (e, ~0, sizeof (*e));
  memset (f, ~0, sizeof (*f));
  memset (g, ~0, sizeof (*g));
  memset (h, ~0, sizeof (*h));
  memset (i, ~0, sizeof (*i));
  memset (j, ~0, sizeof (*j));
  memset (k, ~0, sizeof (*k));
  memset (l, ~0, sizeof (*l));
  memset (m, ~0, sizeof (*m));
  memset (n, ~0, sizeof (*n));
}

__attribute__((noipa)) void
prepare (void)
{
  struct A a;
  struct B b, c, d, e, f, g;
  union U h, i, j, k;
  struct D l, m, n;
  set (&a, &b, &c, &d, &e, &f, &g, &h, &i, &j, &k, &l, &m, &n);
}

int
main ()
{
  prepare ();
  test ();
}
