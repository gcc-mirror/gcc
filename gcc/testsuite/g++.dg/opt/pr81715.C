// PR sanitizer/81715
// { dg-do compile }
// Verify the variables for inlined foo parameters are reused
// { dg-options "-O2 -Wframe-larger-than=16384" }
// { dg-require-effective-target size20plus }

struct S { int a, b, c, d, e; char f[1024]; };
void baz (int *, int *, int *, struct S *, int *, int *);

static inline struct S
foo (int a, int b, int c, struct S d, int e, int f)
{
  struct S s;
  baz (&a, &b, &c, &d, &e, &f);
  s = d;
  return s;
}

struct S g[64];

void
bar (int a, int b, int c, struct S d, int e, int f)
{
#define A(N) \
  g[N+0] = foo (a, b, c, d, e, f);	\
  g[N+1] = foo (a, b, c, d, e, f);	\
  g[N+2] = foo (a, b, c, d, e, f);	\
  g[N+3] = foo (a, b, c, d, e, f);	\
  g[N+4] = foo (a, b, c, d, e, f);	\
  g[N+5] = foo (a, b, c, d, e, f);	\
  g[N+6] = foo (a, b, c, d, e, f);	\
  g[N+7] = foo (a, b, c, d, e, f);	\
  foo (a, b, c, d, e, f);		\
  foo (a, b, c, d, e, f)
  A(0); A(8); A(16); A(24);
  A(32); A(40); A(48); A(56);
}
