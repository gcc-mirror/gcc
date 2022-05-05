// Test that we have one EH cleanup region for the whole array
// rather than one for each element.
// { dg-options "-fdump-tree-gimple" }

#if __cplusplus < 201100L
#define NOTHROW throw()
#else
#define NOTHROW noexcept
#endif

struct A
{
  A();
  ~A() NOTHROW;
};

void f()
{
  A a[10] = { };
}

// { dg-final { scan-tree-dump-times "catch" 1 "gimple" } }
