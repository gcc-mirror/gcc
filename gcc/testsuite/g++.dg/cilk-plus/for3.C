/* { dg-do compile } */
/* { dg-options "-O3 -fcilkplus" } */

// Test storage classes in the initialization of a <#pragma simd> for
// loop.

int *a, *b;

void foo()
{
#pragma simd
  for (int tt=5; tt < 10; ++tt)
    {
      a[tt] = b[tt];
      if (tt == 8)
	throw 1; /* { dg-error "throw expressions are not allowed" } */
    }
}
