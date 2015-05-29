// { dg-do compile }
// { dg-skip-if "" { *-*-* } { "-flto" } { "" } }
// { dg-options "-fsanitize=vptr -O2 -fdump-tree-optimized" }

struct S { virtual ~S (); int i; };

int *
f1 (S *p)
{
  return &p->i;
}

int *
f2 (S *p)
{
  return &*&p->i;
}

int &
f3 (S *p)
{
  return p->i;
}

// { dg-final { scan-tree-dump-times "__ubsan_handle_dynamic_type_cache_miss" 0 "optimized" } }
