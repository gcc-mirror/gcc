/* PR tree-optimization/86196 - Bogus -Wrestrict on memcpy between array
   elements at unequal indices
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

extern void* memcpy (void*, const void*, size_t);

struct S
{
  int n;
  void * p;
};

/* Test case submitted in the PR.  */

void pr86196_c0 (struct S * a, size_t n)
{
  for (size_t i = 0, j = 0; i != n; ++i)
    {
      if (a[i].n == 0)
	{
	  if (i != j)
	    memcpy (&a[j], &a[i], sizeof (struct S));   /* { dg-bogus "\\\[-Wrestrict" } */
	  ++j;
	}
    }
}

/* Reduced test case.  */

void pr86196_c1 (struct S *a, int i, int j)
{
  if (i != j)
    memcpy (&a[j], &a[i], sizeof (struct S));   /* { dg-bogus "\\\[-Wrestrict" } */
}
