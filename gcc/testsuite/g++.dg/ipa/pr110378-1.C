/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-sra -fdump-tree-optimized-slim"  } */

/* Test that even though destructors end with clobbering all of *this, it
   should not prevent IPA-SRA.  */

namespace {

  class foo
  {
  public:
    short move_offset_of_a;
    int *a;
    foo(int c)
    {
      a = new int[c];
      a[0] = 4;
    }
    __attribute__((noinline)) ~foo();
    int f ()
    {
      return a[0] + 1;
    }
  };

  volatile int v1 = 4;

  __attribute__((noinline)) foo::~foo()
  {
    delete[] a;
    return;
  }


}

volatile int v2 = 20;

int test (void)
{
  foo shouldnotexist(v2);
  v2 = shouldnotexist.f();
  return 0;
}


/* { dg-final { scan-ipa-dump "Will split parameter 0" "sra"  } } */
/* { dg-final { scan-tree-dump-not "shouldnotexist" "optimized" } } */
