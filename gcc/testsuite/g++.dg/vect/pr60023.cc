// PR tree-optimization/60023
// { dg-do compile }
// { dg-additional-options "-O3 -std=c++11 -fnon-call-exceptions" }
// { dg-additional-options "-mavx2" { target { i?86-*-* x86_64-*-* } } }

struct A { A (); ~A (); };

void
f1 (int *p, int *q, int *r) noexcept (true)
{
  int i;
  for (i = 0; i < 1024; i++)
    if (r[i])
      p[i] = q[i] + 1;
}

void
f2 (int *p, int *q, int *r)
{
  int i;
  for (i = 0; i < 1024; i++)
    if (r[i])
      p[i] = q[i] + 1;
}

void
f3 (int *p, int *q) noexcept (true)
{
  int i;
  for (i = 0; i < 1024; i++)
    p[i] = q[i] + 1;
}

void
f4 (int *p, int *q)
{
  int i;
  for (i = 0; i < 1024; i++)
    p[i] = q[i] + 1;
}

void
f5 (int *p, int *q, int *r) noexcept (true)
{
  int i;
  A a;
  for (i = 0; i < 1024; i++)
    if (r[i])
      p[i] = q[i] + 1;
}

void
f6 (int *p, int *q, int *r)
{
  int i;
  A a;
  for (i = 0; i < 1024; i++)
    if (r[i])
      p[i] = q[i] + 1;
}

void
f7 (int *p, int *q) noexcept (true)
{
  int i;
  A a;
  for (i = 0; i < 1024; i++)
    p[i] = q[i] + 1;
}

void
f8 (int *p, int *q)
{
  int i;
  A a;
  for (i = 0; i < 1024; i++)
    p[i] = q[i] + 1;
}

// { dg-final { cleanup-tree-dump "vect" } }
