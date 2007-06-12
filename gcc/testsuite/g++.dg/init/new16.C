// { dg-do run }
// { dg-options "-O2 -fstrict-aliasing" }

// Test that we don't let TBAA reorder an assignment across a
// placement new.
// See PR 29286.

typedef __SIZE_TYPE__ size_t;

inline void* operator new(size_t, void* __p) throw() { return __p; }

void __attribute__((noinline)) bar() {}

long __attribute__((noinline)) foo(double *p, int n)
{
  long *f;
  for (int i=0; i<n; ++i)
  {
    int *l = (int *)p;
    *l = 0;
    f = new (p) long;
    *f = -1;
  }
  bar ();
  return *f;
}

extern "C" void abort(void);
int main()
{
  union {
    int i;
    long l;
  } u;
  if (foo((double *)&u, 1) != -1)
    abort ();
  return 0;
}
