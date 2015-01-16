/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int *a, *b, c;

void foo()
{
#pragma simd
  for (int i=0; i < 1000; ++i)
    {
      a[i] = b[i];
      if (c == 5)
	return; /* { dg-error "invalid branch to/from Cilk Plus structured block" } */
    }
}

void bar()
{
#pragma simd
  for (int i=0; i < 1000; ++i)
    {
    lab:
      a[i] = b[i];
    }
  if (c == 6)
    goto lab; /* { dg-error "invalid entry to Cilk Plus structured block" } */
}
