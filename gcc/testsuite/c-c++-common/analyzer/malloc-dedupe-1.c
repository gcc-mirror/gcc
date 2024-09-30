/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>

extern void foo (void);
extern void bar (void);

void test_1 (int flag, int n);

void caller_1_of_test_1 (int n)
{
  test_1 (1, n); /* { dg-bogus "test_1" } */
  test_1 (0, n); /* { dg-bogus "test_1" } */
}

void __attribute__((noinline))
test_1 (int flag, int n)
{
  int *ptr = (int *)malloc (sizeof (int));

  if (flag)
    {
      int i;
      for (i = 0; i < n; i++)
	foo ();
    }
  else
    bar ();

  free (ptr);
  free (ptr); /* { dg-warning "double-'free'" } */
  /* FIXME: we get duplicates intraprocedurally, as there are two paths
     through the function.
     The calls in test_2 also generate additional duplicates.
     How to verify lack of duplicates?
     Putting a bogus on the interprocedual one detects that, at least.  */

  if (flag)
    foo ();
  else
    bar ();
}

void caller_2_of_test_1 (int n)
{
  test_1 (1, n); /* { dg-bogus "test_1" } */
  test_1 (0, n); /* { dg-bogus "test_1" } */
}
