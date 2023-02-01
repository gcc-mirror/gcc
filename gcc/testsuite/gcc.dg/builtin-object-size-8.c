/* { dg-do run } */
/* { dg-options "-O2" } */

#include "builtin-object-size-common.h"

union A
{
  int a1;
  char a2[3];
};

union B
{
  long long b1;
  union A b2;
};

struct C
{
  int c1;
  union A c2;
};

struct D
{
  int d1;
  union B d2;
};

union E
{
  struct C e1;
  char e2[3];
};

union F
{
  int f1;
  struct D f2;
};

struct G
{
  union A g1;
  char g2;
};

struct H
{
  int h1;
  union E h2;
};

#define T(X, S0, S1) \
  if (__builtin_object_size (X, 0) != (S0))	\
    FAIL ();					\
  if (__builtin_object_size (X, 1) != (S1))	\
    FAIL ();					\
  if (__builtin_object_size (X, 2) != (S0))	\
    FAIL ();					\
  if (__builtin_object_size (X, 3) != (S1))	\
    FAIL ()
#define TS(X, S0) T(&X, S0, sizeof (X))
#define TA(X, S0, S1) \
  T(X, S0, S1); T(&X[0], S0, S1); T(&X[1], (S0) - 1, (S1) - 1)
#define TF(X, S0) TA(X, S0, S0)

int
main (void)
{
  size_t s, o, o2;

  s = sizeof (union A);
  o = 0;
  union A *a1 = malloc (s);
  union A *a2 = malloc (o + 212);
  TS (a1->a1, s);
  TF (a1->a2, s);
  s = o + 212;
  TS (a2->a1, s);
  TF (a2->a2, s);
  free (a2);
  free (a1);

  s = sizeof (union B);
  o = 0;
  union B *b1 = malloc (s);
  union B *b2 = malloc (o + 212);
  TS (b1->b1, s);
  TS (b1->b2.a1, s);
  TF (b1->b2.a2, s);
  s = o + 212;
  TS (b2->b1, s);
  TS (b2->b2.a1, s);
  TF (b2->b2.a2, s);
  free (b2);
  free (b1);

  s = sizeof (struct C);
  o = __builtin_offsetof (struct C, c2);
  struct C *c1 = malloc (s);
  struct C *c2 = malloc (o + 212);
  TS (c1->c1, s);
  TS (c1->c2.a1, s - o);
  TF (c1->c2.a2, s - o);
  s = o + 212;
  TS (c2->c1, s);
  TS (c2->c2.a1, s - o);
  TF (c2->c2.a2, s - o);
  free (c2);
  free (c1);

  s = sizeof (struct D);
  o = __builtin_offsetof (struct D, d2);
  struct D *d1 = malloc (s);
  struct D *d2 = malloc (o + 212);
  TS (d1->d1, s);
  TS (d1->d2.b1, s - o);
  TS (d1->d2.b2.a1, s - o);
  TF (d1->d2.b2.a2, s - o);
  s = o + 212;
  TS (d2->d1, s);
  TS (d2->d2.b1, s - o);
  TS (d2->d2.b2.a1, s - o);
  TF (d2->d2.b2.a2, s - o);
  free (d2);
  free (d1);

  s = sizeof (union E);
  o = __builtin_offsetof (union E, e1.c2);
  union E *e1 = malloc (s);
  union E *e2 = malloc (o + 212);
  TS (e1->e1.c1, s);
  TS (e1->e1.c2.a1, s - o);
  TF (e1->e1.c2.a2, s - o);
  TF (e1->e2, s);
  s = o + 212;
  TS (e2->e1.c1, s);
  TS (e2->e1.c2.a1, s - o);
  TF (e2->e1.c2.a2, s - o);
  TF (e2->e2, s);
  free (e2);
  free (e1);

  s = sizeof (union F);
  o = __builtin_offsetof (union F, f2.d2);
  union F *f1 = malloc (s);
  union F *f2 = malloc (o + 212);
  TS (f1->f1, s);
  TS (f1->f2.d1, s);
  TS (f1->f2.d2.b1, s - o);
  TS (f1->f2.d2.b2.a1, s - o);
  TF (f1->f2.d2.b2.a2, s - o);
  s = o + 212;
  TS (f2->f1, s);
  TS (f2->f2.d1, s);
  TS (f2->f2.d2.b1, s - o);
  TS (f2->f2.d2.b2.a1, s - o);
  TF (f2->f2.d2.b2.a2, s - o);
  free (f2);
  free (f1);

  s = sizeof (struct G);
  o = __builtin_offsetof (struct G, g2);
  struct G *g1 = malloc (s);
  struct G *g2 = malloc (o + 212);
  TS (g1->g1.a1, s);
  TA (g1->g1.a2, s, sizeof (g1->g1.a2));
  TS (g1->g2, s - o);
  s = o + 212;
  TS (g2->g1.a1, s);
  TA (g2->g1.a2, s, sizeof (g1->g1.a2));
  TS (g2->g2, s - o);
  free (g2);
  free (g1);

  s = sizeof (struct H);
  o = __builtin_offsetof (struct H, h2);
  o2 = __builtin_offsetof (struct H, h2.e1.c2);
  struct H *h1 = malloc (s);
  struct H *h2 = malloc (o2 + 212);
  TS (h1->h1, s);
  TS (h1->h2.e1.c1, s - o);
  TS (h1->h2.e1.c2.a1, s - o2);
  TA (h1->h2.e1.c2.a2, s - o2, sizeof (h1->h2.e1.c2.a2));
  TF (h1->h2.e2, s - o);
  s = o2 + 212;
  TS (h2->h1, s);
  TS (h2->h2.e1.c1, s - o);
  TS (h2->h2.e1.c2.a1, s - o2);
  TA (h2->h2.e1.c2.a2, s - o2, sizeof (h2->h2.e1.c2.a2));
  TF (h2->h2.e2, s - o);
  free (h2);
  free (h1);

  DONE ();
}
