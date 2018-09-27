/* { dg-options "-O2 -fno-devirtualize --param=profile-func-internal-id=0 -fdump-ipa-profile -fdump-ipa-afdo -Wno-attributes -Wno-coverage-mismatch -Wno-missing-profile" } */
#include "reorder_class1.h"
#include "reorder_class2.h"

int g;

#ifdef _PROFILE_USE
/* Another function not existing
 * in profile-gen  */

__attribute__((noinline)) void
new_func (int i)
{
   g += i;
}
#endif

static __attribute__((always_inline))
void test1 (A *tc)
{
  int i;
  for (i = 0; i < 1000; i++)
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


__attribute__((noinline)) void test_a(A *ap) { test1 (ap); }
__attribute__((noinline)) void test_b(B *bp) { test2 (bp); }


int main()
{
  A* ap = new A();
  B* bp = new B();

  test_a(ap);
  test_b(bp);

#ifdef _PROFILE_USE
  new_func(10);
#endif

}

/* { dg-final-use-not-autofdo { scan-ipa-dump-times "Indirect call -> direct call" 2 "profile" } } */
/* { dg-final-use-autofdo { scan-ipa-dump-times "Indirect call -> direct call" 2 "afdo" } } */
