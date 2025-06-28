/* { dg-options "-O3 -fno-early-inlining -fdump-ipa-afdo-all" } */
/* { dg-require-profiling "-fauto-profile" } */ 

#define N 5000
__attribute__ ((used))
int a[N+1];

__attribute__ ((noinline))
static void
test2(int sz)
{
  a[sz]++;
  asm volatile (""::"m"(a));
}

struct list
{
  struct list *next;
  int val;
};

__attribute__ ((noinline))
static int
test3(volatile struct list l, int v)
{
 a [(l.val + v) % N] = v;
}

__attribute__ ((noinline))
void
test1 (int sz)
{
  volatile struct list l = {0};
  __attribute__ ((noinline))
  void inner(int i)
    {
      if (i % 2)
	test2 (500);
      if (i % 3)
	test3 (l,200);
      else
	test2 (i);
    }
  for (int i = 0; i < N; i++)
    inner(i);

}

int main()
{
  for (int i = 0; i < N; i++)
    {
      test1 (N);
    }
  return 0;
}
/* Profile will have test1.constprop.0 */
/* { dg-final-use-autofdo { scan-ipa-dump "Annotating BB profile of test1" "afdo"} } */
/* { dg-final-use-autofdo { scan-ipa-dump "Annotating BB profile of test2" "afdo"} } */
/* Profile will have test3.constprop.0.isra.0 */
/* { dg-final-use-autofdo { scan-ipa-dump "Annotating BB profile of test3" "afdo"} } */
/* { dg-final-use-autofdo { scan-ipa-dump "Annotating BB profile of inner" "afdo"} } */

