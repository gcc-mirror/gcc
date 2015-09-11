// { dg-do compile }
// { dg-skip-if "" { *-*-* } { "-flto" } { "" } }
// { dg-options "-fsanitize=vptr -O2 -fdump-tree-optimized" }

struct S { virtual ~S (); int i; _Complex int j[5]; };

int
f1 (S *p)
{
  return p->i;
}

int
f2 (S *p)
{
  return *&p->i;
}

_Complex int *
f3 (S *p, S *q)
{
  return &p->j[q->i];
}

int
f4 (S &p, S &q)
{
  return __imag__ p.j[q.i];
}

// { dg-final { scan-tree-dump-times "__ubsan_handle_dynamic_type_cache_miss" 5 "optimized" } }
