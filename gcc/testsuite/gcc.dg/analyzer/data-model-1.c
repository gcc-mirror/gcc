#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <alloca.h>
#include "analyzer-decls.h"

struct foo
{
  int i;
};

/* Fields of a local.  */

void test_1 (void)
{
  struct foo f;
  f.i = 1;
  __analyzer_eval (f.i == 1); /* { dg-warning "TRUE" } */
}

/* Fields of a param.  */

void test_2 (struct foo f)
{
  __analyzer_eval (f.i == 2); /* { dg-warning "UNKNOWN" } */
  f.i = 2;
  __analyzer_eval (f.i == 2); /* { dg-warning "TRUE" } */
}

/* Fields of a param ptr.  */

void test_3 (struct foo *f)
{
  __analyzer_eval (f->i == 3); /* { dg-warning "UNKNOWN" } */
  f->i = 3;
  __analyzer_eval (f->i == 3); /* { dg-warning "TRUE" } */
}

/* Fields of a global ptr.  */
struct foo *global_foo_ptr;

void test_3a (void)
{
  struct foo *tmp = global_foo_ptr;
  __analyzer_eval (global_foo_ptr->i == 3); /* { dg-warning "UNKNOWN" } */
  global_foo_ptr->i = 3;
  __analyzer_eval (global_foo_ptr->i == 3); /* { dg-warning "TRUE" } */
}

/* Pointer to a local.  */

void test_4 (void)
{
  int i;
  int *p = &i;
  i = 1;
  *p = 2;
  __analyzer_eval (i == 2); /* { dg-warning "TRUE" } */
}

/* Local array.  */

void test_5 (void)
{
  int a[10];
  a[3] = 5; /* ARRAY_REF.  */
  __analyzer_eval (a[3] == 5); /* { dg-warning "TRUE" } */
}

/* Local array, but using an unknown index.  */

void test_5a (int idx)
{
  int a[10];
  a[idx] = 5; /* ARRAY_REF.  */
  __analyzer_eval (a[idx] == 5); /* { dg-warning "TRUE" } */
}

/* Array passed in as a param.  */

void test_6 (int a[10])
{
  /* POINTER_PLUS_EXPR then a MEM_REF.  */
  __analyzer_eval (a[3] == 42); /* { dg-warning "UNKNOWN" } */
  a[3] = 42;
  __analyzer_eval (a[3] == 42); /* { dg-warning "TRUE" } */
}

/* Array passed in as a param ptr.  */

void test_7 (int *a)
{
  __analyzer_eval (a[3] == 42); /* { dg-warning "UNKNOWN" } */
  a[3] = 42;
  __analyzer_eval (a[3] == 42); /* { dg-warning "TRUE" } */
}

/* Globals.  */

int glob_a;

void test_10 (void)
{
  __analyzer_eval (glob_a == 42); /* { dg-warning "UNKNOWN" } */
  glob_a = 42;
  __analyzer_eval (glob_a == 42); /* { dg-warning "TRUE" } */
}

/* malloc.  */

void test_11 (void)
{
  void *p = malloc (256);
  void *q = malloc (256);

  /* malloc results should be unique.  */
  __analyzer_eval (p == q); /* { dg-warning "FALSE" } */
  __analyzer_eval (p != q); /* { dg-warning "TRUE" } */
  __analyzer_eval (p <= q); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (p >= q); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (p < q); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (p > q); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (p == p); /* { dg-warning "TRUE" } */
  __analyzer_eval (p != p); /* { dg-warning "FALSE" } */
  __analyzer_eval (p <= p); /* { dg-warning "TRUE" } */
  __analyzer_eval (p >= p); /* { dg-warning "TRUE" } */
  __analyzer_eval (p < p); /* { dg-warning "FALSE" } */
  __analyzer_eval (p > p); /* { dg-warning "FALSE" } */

  free (p);
  free (q);
  // TODO: mark freed memory as freed
  //__analyzer_break ();
}

/* alloca.  */

void test_12 (void)
{
  void *p = alloca (256);
  void *q = alloca (256);

  /* alloca results should be unique.  */
  __analyzer_eval (p == q); /* { dg-warning "FALSE" } */

  // FIXME: complain about uses of poisoned values
}

/* Use of uninit value.  */
int test_12a (void)
{
  int i;
  return i; // FIXME: do we see the return stmt?
}

void test_12b (void *p, void *q)
{
  __analyzer_eval (p == q); /* { dg-warning "UNKNOWN" } */
}

int test_12c (void)
{
  int i;
  int j;

  j = i; // FIXME: should complain about this

  return j;
}

struct coord
{
  long x;
  long y;
};

int test_12d (struct coord c)
{
  struct coord d;
  d = c;
  __analyzer_eval (d.x == c.x); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "actual" { target *-*-* } .-1 } */
  /* TODO(xfail): c and d share the same unknown value of type "coord", but
     attempts to access the fields lead to different unknown values.  */

  __analyzer_eval (d.y == c.y); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "actual" { target *-*-* } .-1 } */
  // TODO(xfail): likewise

  __analyzer_eval (d.x == d.y); /* { dg-warning "UNKNOWN" } */
  /* d and c share an unknown value of type "struct coord".
     But d.x and d.y should be different unknown values (although they inherit
     from d's region).  */
}

/* Nested structs.  */

struct outer
{
  struct middle {
    struct inner {
      float f;
    } in;
  } mid;
};

void test_13 (struct outer *o)
{
  __analyzer_eval (o->mid.in.f == 0.f); /* { dg-warning "UNKNOWN" } */
  o->mid.in.f = 0.f;
  __analyzer_eval (o->mid.in.f == 0.f); /* { dg-warning "TRUE" "PR 93356" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "disabled float comparisons" { target *-*-* } .-1 } */
}

void test_14 (struct outer o)
{
  __analyzer_eval (o.mid.in.f == 0.f); /* { dg-warning "UNKNOWN" } */
  o.mid.in.f = 0.f;
  __analyzer_eval (o.mid.in.f == 0.f); /* { dg-warning "TRUE" "PR 93356" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "disabled float comparisons" { target *-*-* } .-1 } */
}

void test_15 (const char *str)
{
  char ch = str[0];
  __analyzer_eval (ch == 'a'); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (ch == str[0]); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail)

  ch = 'a';
  __analyzer_eval (ch == 'a'); /* { dg-warning "TRUE" } */
  __analyzer_eval (str[0] == 'a'); /* { dg-warning "UNKNOWN" } */
}

void test_16 (void)
{
  const char *msg = "hello world";

  __analyzer_eval (msg != NULL); /* { dg-warning "TRUE" } */

  __analyzer_eval (msg[0] == 'h'); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail)

  __analyzer_eval (msg[1] == 'e'); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail)

  __analyzer_eval (strlen (msg) == 11); /* { dg-warning "TRUE" } */
}

static const char *__attribute__((noinline))
get_hello_world (void)
{
  return "hello world";
}

void test_16_alt (void)
{
  const char *msg = get_hello_world ();

  __analyzer_eval (msg != NULL); /* { dg-warning "TRUE" } */

  __analyzer_eval (msg[0] == 'h'); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail)

  __analyzer_eval (msg[1] == 'e'); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail)

  __analyzer_eval (strlen (msg) == 11); /* { dg-warning "TRUE" } */
}

void test_16a (const char *msg)
{
  __analyzer_eval (strlen (msg) == 11); /* { dg-warning "UNKNOWN" } */
}

void test_16b (const char *msg)
{
  __analyzer_eval (strlen (msg) == strlen (msg)); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail)
}

extern int unknown_result (void);

void test_16c (int i)
{
  int j;

  j = i;
  __analyzer_eval (j == i); /* { dg-warning "TRUE" } */

  j = unknown_result ();
  __analyzer_eval (j == i); /* { dg-warning "UNKNOWN" } */
}

void test_16c_a (void)
{
  int i, j;

  i = unknown_result ();
  j = unknown_result ();
  __analyzer_eval (i == j); /* { dg-warning "UNKNOWN" } */
}

int global_int_16d;

void test_16d (int i)
{
  global_int_16d = i;
  __analyzer_eval (global_int_16d == i); /* { dg-warning "TRUE" } */

  global_int_16d = unknown_result ();
  __analyzer_eval (global_int_16d == i); /* { dg-warning "UNKNOWN" } */
}

extern void might_write_to (int *);

void test_16e (int i)
{
  int j;

  j = i;
  __analyzer_eval (j == i); /* { dg-warning "TRUE" } */

  might_write_to (&j);
  __analyzer_eval (j == i); /* { dg-warning "UNKNOWN" } */
}

/* TODO: and more complicated graph-like examples, where anything that's
   reachable from the pointer might be modified.  */

void test_17 (int i)
{
  int j = 42;
  __analyzer_eval (j == 42); /* { dg-warning "TRUE" } */

  __analyzer_eval (i == j); /* { dg-warning "UNKNOWN" } */
  i = j;
  __analyzer_eval (i == j); /* { dg-warning "TRUE" } */
}

void test_18 (int i)
{
  int j;
  __analyzer_eval (i == 42); /* { dg-warning "UNKNOWN" } */

  j = i;

  __analyzer_eval (i == j); /* { dg-warning "TRUE" } */
  __analyzer_eval (i >= j); /* { dg-warning "TRUE" } */
  __analyzer_eval (i <= j); /* { dg-warning "TRUE" } */

  __analyzer_eval (i != j); /* { dg-warning "FALSE" } */
  __analyzer_eval (i > j); /* { dg-warning "FALSE" } */
  __analyzer_eval (i < j); /* { dg-warning "FALSE" } */
}

void test_19 (void)
{
  int i, j;
  /* Compare two uninitialized locals.  */
    __analyzer_eval (i == j); /* { dg-warning "UNKNOWN" } */
}

void test_20 (int i, int j)
{
  __analyzer_eval (i + 1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i + j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i - 1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i - j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i * 2); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i * j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i / 2); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i / j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i % 2); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i % j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i & 1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i & j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i | 1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i | j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i ^ 1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i ^ j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i >> 1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i >> j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i << 1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i << j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i && 0); /* { dg-warning "FALSE" } */
  __analyzer_eval (i && 1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i && j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i || 0); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (i || 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (i || j); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval (~i); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (-i); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (+i); /* { dg-warning "UNKNOWN" } */

  /* Anything added above should be added to the next function also.  */
}

/* As above, but where the values are known due to the region model,
   but not known to GCC's regular optimizations (folding and SSA).  */

void test_21 (void)
{
  int i, j, zero;
  int *pi = &i;
  int *pj = &j;
  int *pzero = &zero;
  *pi = 5;
  *pj = 3;
  *pzero = 0;

  __analyzer_eval (i + j == 8); /* { dg-warning "TRUE" } */
  __analyzer_eval (i - j == 2); /* { dg-warning "TRUE" } */
  __analyzer_eval (i * j == 15); /* { dg-warning "TRUE" } */
  __analyzer_eval (i / j == 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (i % j == 2); /* { dg-warning "TRUE" } */

  /* Division by zero.  */
  // TODO: should we warn for this?
  __analyzer_eval (i / zero); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (i % zero); /* { dg-warning "UNKNOWN" } */

  __analyzer_eval ((i & 1) == (5 & 1)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i & j) == (5 & 3)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i | 1) == (5 | 1)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i | j) == (5 | 3)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i ^ 1) == (5 ^ 1)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i ^ j) == (5 ^ 3)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i >> 1) == (5 >> 1)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i >> j) == (5 >> 3)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i << 1) == (5 << 1)); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i << j) == (5 << 3)); /* { dg-warning "TRUE" } */
  __analyzer_eval (i && 0); /* { dg-warning "FALSE" } */
  __analyzer_eval (i && 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (i && j); /* { dg-warning "TRUE" } */

  __analyzer_eval (i || 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (i || 1); /* { dg-warning "TRUE" } */
  __analyzer_eval (i || j); /* { dg-warning "TRUE" } */

  __analyzer_eval (~i == ~5); /* { dg-warning "TRUE" } */
  __analyzer_eval (-i == -5); /* { dg-warning "TRUE" } */
  __analyzer_eval (+i == +5); /* { dg-warning "TRUE" } */
}

void test_22 (int i, int j)
{
  __analyzer_eval (i + j == i + j); /* { dg-warning "TRUE" } */
  // FIXME: this is getting folded; can we build a non-folded equivalent?
}

void test_23 (struct foo *f, struct foo *g)
{
  int i, j, k;
  i = f->i + g->i;
  j = f->i + g->i;
  k = f->i * g->i;
  __analyzer_eval (i == j); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  /* TODO(xfail): we'd need to record that the two unknown values are both
     the sum of the two unknown input values (and thus are the same); not
     yet sure if we want arbitrary expression trees in the representation
     (analysis termination concerns).  */

  __analyzer_eval (i == k); /* { dg-warning "UNKNOWN" } */
}

void test_24 (struct foo *f)
{
  struct foo g;
  g.i = 42;
  __analyzer_eval (g.i == 42); /* { dg-warning "TRUE" } */

  /* Overwriting a whole struct should invalidate our knowledge
     about fields within it.  */
  g = *f;
  __analyzer_eval (g.i == 42); /* { dg-warning "UNKNOWN" "desired" { xfail *-*-* } } */
  /* { dg-warning "TRUE" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail)
}

void test_25 (struct foo *f)
{
  struct foo g;
  g.i = 42;
  f->i = 43;
  __analyzer_eval (f->i == 43); /* { dg-warning "TRUE" } */
  __analyzer_eval (g.i == 42); /* { dg-warning "TRUE" } */

  /* Overwriting a whole struct where we know things about the
     source value should update our knowledge about fields within
     the dest value.  */
  g = *f;
  __analyzer_eval (g.i == 43); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "FALSE" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail)
}

void test_26 (struct coord *p, struct coord *q)
{
  p->x = 42;
  q->y = 17;
  __analyzer_eval (p->x == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (p->y); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (q->x); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (q->y == 17); /* { dg-warning "TRUE" } */

  /* Overwriting a whole struct where we know some things about the
     source value should update our knowledge about fields within
     the dest value.  */
  *p = *q;
  __analyzer_eval (p->x); /* { dg-warning "UNKNOWN" "desired" { xfail *-*-* } } */
  /* { dg-warning "TRUE" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail): should have been overwritten
  __analyzer_eval (p->y == 17); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail): should have been overwritten with q->y

  __analyzer_eval (q->x); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (q->y == 17); /* { dg-warning "TRUE" } */
}

void test_27 (struct coord *p)
{
  memset (p, 0, sizeof (struct coord));
  __analyzer_eval (p->x == 0); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail):
  __analyzer_eval (p->y == 0); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail):
}

void test_28 (struct coord *p)
{
  memset (p, 0, sizeof (struct coord) * 10);
  __analyzer_eval (p[0].x == 0); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail):
  __analyzer_eval (p[0].y == 0); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail):

  __analyzer_eval (p[9].x == 0); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail):
  __analyzer_eval (p[9].y == 0); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail):

  __analyzer_eval (p[10].x == 0); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (p[10].y == 0); /* { dg-warning "UNKNOWN" } */
}

void test_29 (struct coord *p)
{
  struct coord *q;

  p[0].x = 100024;
  p[0].y = 100025;

  p[7].x = 107024;
  p[7].y = 107025;

  p[9].x = 109024;
  p[9].y = 109025;

  __analyzer_eval (p[0].x == 100024); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[0].y == 100025); /* { dg-warning "TRUE" } */

  __analyzer_eval (p[7].x == 107024); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[7].y == 107025); /* { dg-warning "TRUE" } */

  __analyzer_eval (p[9].x == 109024); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[9].y == 109025); /* { dg-warning "TRUE" } */

  __analyzer_eval (p[10].x == 0); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (p[10].y == 0); /* { dg-warning "UNKNOWN" } */

  q = &p[7];

  __analyzer_eval (q->x == 107024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q->y == 107025); /* { dg-warning "TRUE" } */

  __analyzer_eval (q[2].x == 109024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q[2].y == 109025); /* { dg-warning "TRUE" } */

  q += 2;

  __analyzer_eval (q->x == 109024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q->y == 109025); /* { dg-warning "TRUE" } */

  __analyzer_eval (q[-2].x == 107024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q[-2].y == 107025); /* { dg-warning "TRUE" } */

  q -= 2;

  __analyzer_eval (q->x == 107024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q->y == 107025); /* { dg-warning "TRUE" } */
}

void test_29a (struct coord p[])
{
  struct coord *q;

  p[0].x = 100024;
  p[0].y = 100025;

  p[7].x = 107024;
  p[7].y = 107025;

  p[9].x = 109024;
  p[9].y = 109025;

  __analyzer_eval (p[0].x == 100024); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[0].y == 100025); /* { dg-warning "TRUE" } */

  __analyzer_eval (p[7].x == 107024); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[7].y == 107025); /* { dg-warning "TRUE" } */

  __analyzer_eval (p[9].x == 109024); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[9].y == 109025); /* { dg-warning "TRUE" } */

  __analyzer_eval (p[10].x == 0); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (p[10].y == 0); /* { dg-warning "UNKNOWN" } */

  q = &p[7];

  __analyzer_eval (q->x == 107024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q->y == 107025); /* { dg-warning "TRUE" } */

  __analyzer_eval (q[2].x == 109024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q[2].y == 109025); /* { dg-warning "TRUE" } */

  q += 2;

  __analyzer_eval (q->x == 109024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q->y == 109025); /* { dg-warning "TRUE" } */

  __analyzer_eval (q[-2].x == 107024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q[-2].y == 107025); /* { dg-warning "TRUE" } */

  q -= 2;

  __analyzer_eval (q->x == 107024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q->y == 107025); /* { dg-warning "TRUE" } */
}

void test_29b (void)
{
  struct coord p[11];
  struct coord *q;

  p[0].x = 100024;
  p[0].y = 100025;

  p[7].x = 107024;
  p[7].y = 107025;

  p[9].x = 109024;
  p[9].y = 109025;

  __analyzer_eval (p[0].x == 100024); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[0].y == 100025); /* { dg-warning "TRUE" } */

  __analyzer_eval (p[7].x == 107024); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[7].y == 107025); /* { dg-warning "TRUE" } */

  __analyzer_eval (p[9].x == 109024); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[9].y == 109025); /* { dg-warning "TRUE" } */

  __analyzer_eval (p[10].x == 0); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (p[10].y == 0); /* { dg-warning "UNKNOWN" } */

  q = &p[7];

  __analyzer_eval (q->x == 107024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q->y == 107025); /* { dg-warning "TRUE" } */

  __analyzer_eval (q[2].x == 109024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q[2].y == 109025); /* { dg-warning "TRUE" } */

  q += 2;

  __analyzer_eval (q->x == 109024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q->y == 109025); /* { dg-warning "TRUE" } */

  __analyzer_eval (q[-2].x == 107024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q[-2].y == 107025); /* { dg-warning "TRUE" } */

  q -= 2;

  __analyzer_eval (q->x == 107024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q->y == 107025); /* { dg-warning "TRUE" } */
}

void test_29c (int len)
{
  struct coord p[len];
  struct coord *q;

  p[0].x = 100024;
  p[0].y = 100025;

  p[7].x = 107024;
  p[7].y = 107025;

  p[9].x = 109024;
  p[9].y = 109025;

  __analyzer_eval (p[0].x == 100024); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[0].y == 100025); /* { dg-warning "TRUE" } */

  __analyzer_eval (p[7].x == 107024); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[7].y == 107025); /* { dg-warning "TRUE" } */

  __analyzer_eval (p[9].x == 109024); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[9].y == 109025); /* { dg-warning "TRUE" } */

  __analyzer_eval (p[10].x == 0); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (p[10].y == 0); /* { dg-warning "UNKNOWN" } */

  q = &p[7];

  __analyzer_eval (q->x == 107024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q->y == 107025); /* { dg-warning "TRUE" } */

  __analyzer_eval (q[2].x == 109024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q[2].y == 109025); /* { dg-warning "TRUE" } */

  q += 2;

  __analyzer_eval (q->x == 109024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q->y == 109025); /* { dg-warning "TRUE" } */

  __analyzer_eval (q[-2].x == 107024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q[-2].y == 107025); /* { dg-warning "TRUE" } */

  q -= 2;

  __analyzer_eval (q->x == 107024); /* { dg-warning "TRUE" } */
  __analyzer_eval (q->y == 107025); /* { dg-warning "TRUE" } */
}

void test_30 (void *ptr)
{
  struct coord *p = (struct coord *)ptr;
  struct coord *q = (struct coord *)ptr;

  p->x = 42;

  __analyzer_eval (p->x == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (q->x == 42); /* { dg-warning "TRUE" } */
}

void test_31 (unsigned i)
{
  int j, k;

  j = i < 100 ? i : 100; /* MIN_EXPR.  */
  k = i < 100 ? 100 : i; /* MAX_EXPR.  */
}

enum color
{
  RED,
  GREEN,
  BLUE
};

void test_32 (enum color c)
{
  __analyzer_eval (c == GREEN); /* { dg-warning "UNKNOWN" } */

  c = RED;

  __analyzer_eval (c == RED); /* { dg-warning "TRUE" } */
  __analyzer_eval (c == GREEN); /* { dg-warning "FALSE" } */
}

void test_33 (void)
{
  static int s;

  __analyzer_eval (s == 42); /* { dg-warning "UNKNOWN" } */

  s = 42;

  __analyzer_eval (s == 42); /* { dg-warning "TRUE" } */
}

static int __attribute__((noinline))
only_called_by_test_34 (int parm)
{
  __analyzer_eval (parm == 42); /* { dg-warning "TRUE" } */

  return parm * 2;
}

void test_34 (void)
{
  int result = only_called_by_test_34 (42);
  __analyzer_eval (result == 84); /* { dg-warning "TRUE" } */
}

void test_35 (int i, int j)
{
  __analyzer_eval (&i == &i); /* { dg-warning "TRUE" } */
  __analyzer_eval (&i != &j); /* { dg-warning "TRUE" } */
}

static void __attribute__((noinline))
write_through_ptr (int *dst, int val)
{
  *dst = val;
}

void test_36 (int i)
{
  __analyzer_eval (i == 42); /* { dg-warning "UNKNOWN" } */

  write_through_ptr (&i, 42);

  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */
}

/* Read through uninitialized pointer.  */

int test_37 (void)
{
  int *ptr;
  return *ptr; /* { dg-warning "use of uninitialized value 'ptr'" "uninit-warning-removed" { xfail *-*-* } } */
}

/* Write through uninitialized pointer.  */

void test_37a (int i)
{
  int *ptr;
  *ptr = i; /* { dg-warning "use of uninitialized value 'ptr'" "uninit-warning-removed" { xfail *-*-* } } */
}

// TODO: the various other ptr deref poisonings

/* Read through NULL pointer.  */

int test_38 (void)
{
  int *ptr = NULL;
  return *ptr; /* { dg-warning "dereference of NULL 'ptr'" } */
}

/* Write through NULL pointer.  */

int test_38a (int i)
{
  int *ptr = NULL;
  *ptr = i; /* { dg-warning "dereference of NULL 'ptr'" } */
}

/* Read through non-NULL constant pointer.  */

int test_39 (void)
{
  int *ptr = (int *)0x1000;
  return *ptr;
}

int test_40 (int flag)
{
  int i;
  if (flag)
    i = 43;
  else
    i = 17;

  /* With state-merging, we lose the relationship between 'flag' and 'i'.  */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  if (flag)
    __analyzer_eval (i == 43); /* { dg-warning "UNKNOWN" } */
  else
    __analyzer_eval (i == 17); /* { dg-warning "UNKNOWN" } */
}

struct link
{
  struct link *next;
  int f;
};

/* Traversing a singly-linked list.  */

void foo (struct link *in)
{
  struct link a;
  struct link b;
  struct link c;
  a.next = &b;
  b.next = &c;
  in->next = &a;
  c.f = 42;
  __analyzer_eval (in->next->next->next->f == 42);  /* { dg-warning "TRUE" } */
}

union u
{
  int i;
  int *ptr;
};

void test_41 (void)
{
  union u u;
  u.i = 42;
  __analyzer_eval (u.i == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (u.ptr == NULL); /* { dg-warning "UNKNOWN" } */

  /* Writes to a union member should invalidate knowledge about other members.  */
  u.ptr = NULL;
  __analyzer_eval (u.ptr == NULL); /* { dg-warning "TRUE" } */
  __analyzer_eval (u.i == 42); /* { dg-warning "UNKNOWN" } */
}

void test_42 (void)
{
  int i;
  float f;
  i = 42;
  f = i;
  __analyzer_eval (f == 42.0); /* { dg-warning "TRUE" "PR 93356" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "disabled float comparisons" { target *-*-* } .-1 } */
}

void test_43 (void)
{
  int i;
  float f;
  f = 42.0f;
  i = f;
  __analyzer_eval (i == 42);  /* { dg-warning "TRUE" } */
}

struct sbits
{
  int b0 : 1;
  int b123 : 3;
  int b456 : 3;
  int b7 : 1;
};

void test_44 (void)
{
  struct sbits bits;
  bits.b0 = 1;
  __analyzer_eval (bits.b0 == 1); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "FALSE" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail): ^^^^

  bits.b456 = 5;
  __analyzer_eval (bits.b456 == 5); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "FALSE" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail): ^^^^
};

struct ubits
{
  unsigned int b0 : 1;
  unsigned int b123 : 3;
  unsigned int b456 : 3;
  unsigned int b7 : 1;
};

/* FIXME: this requires BIT_FIELD_REF to work.  */

void test_45 (void)
{
  struct ubits bits;
  bits.b0 = 1;
  __analyzer_eval (bits.b0 == 1); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail): ^^^^

  bits.b456 = 5;
  __analyzer_eval (bits.b456 == 5); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  // TODO(xfail): ^^^^
};

extern const char *char_ptr;

int test_46 (void)
{
  if (strcmp("literal", char_ptr))
    return 1;
  return 0;
}

char test_47 (void)
{
  static const char* my_version = "1.1.3";
  return my_version[0];
}

unsigned test_48 (unsigned char *p, unsigned char *q)
{
  return (unsigned int)(p - q);
}

typedef struct {
  const char *filename;
  short lineno;
} loc;

static loc loc_last;

void test_49 (void)
{
  loc_last = __extension__(loc) { "", 328 };
  loc_last = __extension__(loc) { "", 333 };
}

void test_50 (void *p, void *q)
{
  __analyzer_eval (p == q); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (p == p); /* { dg-warning "TRUE" } */
  __analyzer_eval (q == q); /* { dg-warning "TRUE" } */
  __analyzer_eval (p == (struct coord *)p); /* { dg-warning "TRUE" } */
  __analyzer_eval (p == (const struct coord *)p); /* { dg-warning "TRUE" } */

  struct coord *cp = (struct coord *)p;
  __analyzer_eval (p == cp); /* { dg-warning "TRUE" } */

  struct coord *cq = (struct coord *)q;
  __analyzer_eval (q == cq); /* { dg-warning "TRUE" } */

  __analyzer_eval (cp == cq); /* { dg-warning "UNKNOWN" } */
}

void test_51 (struct coord c)
{
  struct coord d;
  memcpy (&d, &c, sizeof (struct coord));
  __analyzer_eval (c.x == d.x); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  __analyzer_eval (c.y == d.y); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
}

struct big
{
  int ia[1024];  
};

void test_52 (struct big b)
{
  struct big d;
  memcpy (&d, &b, sizeof (struct big));
  __analyzer_eval (b.ia[0] == d.ia[0]); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
}

void test_53 (const char *msg)
{
  (void)fprintf(stderr, "LOG: %s", msg);
}
