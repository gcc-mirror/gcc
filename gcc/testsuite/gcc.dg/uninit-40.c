/* PR tree-optimization/98597 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vectorize -Wuninitialized" } */

union U { double d; int i; float f; };
struct S { char a; int b; char c; unsigned d; union U e; int f[3]; unsigned g[3]; };
struct T { char t; struct S u; int v; };
typedef short V[2][2];
void baz (V *);

static inline int
bar (char *p)
{
  return *(int *) p;
}

void
foo (int *q)
{
  struct T t;
  t.t = 1;
  t.u.c = 2;
  char *pt = (char *) &t;
  q[0] = bar (pt + __builtin_offsetof (struct T, u.b));	/* { dg-warning "'t\\.u\\.b' is used uninitialized" } */
  q[1] = bar (pt + __builtin_offsetof (struct T, u.e));	/* { dg-warning "'\\*\\(int \\*\\)\\(\\(char \\*\\)&t \\+ offsetof\\(struct T, u\\.e\\)\\)' is used uninitialized" } */
  q[2] = bar (pt + __builtin_offsetof (struct T, v));	/* { dg-warning "'t\\.v' is used uninitialized" } */
  q[3] = bar (pt + __builtin_offsetof (struct T, u.d));	/* { dg-warning "'\\*\\(int \\*\\)\\(\\(char \\*\\)&t \\+ offsetof\\(struct T, u\\.d\\)\\)' is used uninitialized" } */
  q[4] = bar (pt + __builtin_offsetof (struct T, u.f[2])); /* { dg-warning "'t\\.u\\.f\\\[2\\\]' is used uninitialized" } */
  q[5] = bar (pt + __builtin_offsetof (struct T, u.g[2])); /* { dg-warning "'\\*\\(int \\*\\)\\(\\(char \\*\\)&t \\+ offsetof\\(struct T, u\\.g\\\[2\\\]\\)\\)' is used uninitialized" } */
  int s[3];
  s[0] = 1;
  char *ps = (char *) s;
  q[6] = bar (ps + sizeof (int));			/* { dg-warning "'s\\\[1\\\]' is used uninitialized" } */
  unsigned w[2][2];
  w[0][0] = 1;
  char *pw = (char *) w;
  q[7] = bar (pw + 3 * sizeof (unsigned));		/* { dg-warning "'\\*\\(int \\*\\)\\(&w\\\[1\\\]\\\[1\\\]\\)' is used uninitialized" } */
  struct T x[3][3];
  x[0][0].t = 1;
  char *px = (char *) x;
  q[8] = bar (px + 5 * sizeof (struct T) + __builtin_offsetof (struct T, u.b));	/* { dg-warning "'x\\\[1\\\]\\\[2\\\]\\.u\\.b' is used uninitialized" } */
  q[9] = bar (px + 6 * sizeof (struct T) + __builtin_offsetof (struct T, u.d));	/* { dg-warning "'\\*\\(int \\*\\)\\(\\(char \\*\\)&x\\\[2\\\]\\\[0\\\] \\+ offsetof\\(struct T, u\\.d\\)\\)' is used uninitialized" } */
#if defined(__i386__) || defined(__x86_64__)
  /* memcpy folding is too target dependent to test it everywhere.  */
  V u[2], v[2];
  u[0][0][0] = 1;
  __builtin_memcpy (&v[1], &u[1], sizeof (V));		/* { dg-warning "'\\*\\(\(long \)?long unsigned int \\*\\)\\(&u\\\[1\\\]\\\[0\\\]\\\[0\\\]\\)' is used uninitialized" "" { target i?86-*-* x86_64-*-* } } */
  baz (&v[1]);
#endif
}
