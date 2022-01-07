// PR c++/52320
// { dg-do run }

#if DEBUG
extern "C" int printf (const char *, ...);
#define FUNCTION_NAME __PRETTY_FUNCTION__
#define TRACE_FUNCTION printf ("%p->%s\n", this, FUNCTION_NAME);
#else
#define TRACE_FUNCTION 
#endif
int c,d;
#define TRACE_CTOR TRACE_FUNCTION ++c
#define TRACE_DTOR TRACE_FUNCTION ++d

int throw_at = 0;

struct A {
  A() { int i = c+1; if (i == throw_at) throw i; TRACE_CTOR; }
  A(int i) { if (i == throw_at) throw i; TRACE_CTOR; }
  A(const A&) { throw 10; }
  A &operator=(const A&) { throw 11; return *this; }
  ~A() { TRACE_DTOR; }
};

int fails;

void try_idx (int i)
{
#if DEBUG
  printf ("trying %d\n", i);
#endif
  throw_at = i;
  c = d = 0;
  int t = 10;
  try {
    struct X {
      A e1[2], e2;
    }
    x2[3] = { { 1, 2, 3 }, { 4, 5, 6 } };
  } catch (int x) { t = x; }
  if (t != i || c != d || c != i-1)
    {
#if DEBUG
      printf ("%d FAIL\n", i);
#endif
      ++fails;
    }
}

int main()
{
  for (int i = 1; i <= 10; ++i)
    try_idx (i);

  return fails;
}
