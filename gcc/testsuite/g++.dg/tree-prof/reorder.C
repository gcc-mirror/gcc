/* { dg-options "-O2 -fno-devirtualize --param=profile-func-internal-id=0 -fdump-ipa-profile-optimized -fdump-ipa-afdo -Wno-coverage-mismatch -Wno-attributes" } */

#ifdef _PROFILE_USE
#include "reorder_class1.h"
#include "reorder_class2.h"
#else
#include "reorder_class2.h"
#include "reorder_class1.h"
#endif

int g;
static __attribute__((always_inline))
void test1 (A *tc)
{
  int i;
  for (i = 0; i < 1000000; i++)
     g += tc->foo(); 
   if (g<100) g++;
}

static __attribute__((always_inline))
void test2 (B *tc)
{
  int i;
  for (i = 0; i < 1000000; i++)
     g += tc->foo();
}


#ifdef _PROFILE_USE
__attribute__((noinline)) void test_a(A *ap) { test1 (ap); }
__attribute__((noinline)) void test_b(B *bp) { test2 (bp); }
#else
__attribute__((noinline)) void test_b(B *bp) { test2 (bp); }
__attribute__((noinline)) void test_a(A *ap) { test1 (ap); }
#endif

int main()
{
  A* ap = new A();
  B* bp = new B();

  test_a(ap);
  test_b(bp);
}

/* { dg-final-use-not-autofdo { scan-ipa-dump-times "Indirect call -> direct call" 2 "profile" } } */
/* { dg-final-use-autofdo { scan-ipa-dump-times "Indirect call -> direct call" 2 "afdo" } } */
