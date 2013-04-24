/* { dg-do compile } */
/* { dg-options "-O3 -fcilkplus" } */

int *a, *b, c;
void *jmpbuf[10];

void foo()
{
  int i, j;

#pragma simd
  for (i=0; i < 1000; ++i)
    {
      if (c == 5)
	return;	 /* { dg-error "return statments are not allowed" } */
      if (c == 6)
	__builtin_setjmp (jmpbuf); /* { dg-error "calls to setjmp are not allowed" } */
      a[i] = b[i];
    }

#pragma simd
  for (i=0; i < 1000; ++i)
    {
      if (c==5)
	break; /* { dg-error "break statement within" } */
    }
}
