/* PR middle-end/92323 - bogus -Warray-bounds after unrolling despite
   __builtin_unreachable
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

struct S { int a[5]; } s;

void sink (void*);

#pragma GCC optimize "2"

void f_O2 (unsigned n, struct S *p)
{
  for (unsigned i = 1; i < n - 1; ++i)
    s.a[i - 1] = p->a[i];     // { dg-bogus "\\\[-Warray-bounds" }

  if (n < 4 || n > 5)
    __builtin_unreachable ();
}

void g_O2 (unsigned n, struct S *p)
{
  if (n < 4 || n > 5)
    __builtin_unreachable ();

  for (unsigned i = 1; i < n - 1; ++i)
    s.a[i - 1] = p->a[i];
}


// Also exercise -O3 with loop unrolling for good measure.

#pragma GCC optimize "3"

struct T { int a[6]; } t;

void f_O3 (unsigned n, struct T *p)
{
  for (unsigned i = 1; i < n - 1; ++i)
    t.a[i - 1] = p->a[i];     // { dg-bogus "\\\[-Warray-bounds" }

  if (n < 5 || n > 6)
    __builtin_unreachable ();
}

void g_O3 (unsigned n, struct T *p)
{
  if (n < 5 || n > 6)
    __builtin_unreachable ();

  for (unsigned i = 1; i < n - 1; ++i)
    s.a[i - 1] = p->a[i];
}
