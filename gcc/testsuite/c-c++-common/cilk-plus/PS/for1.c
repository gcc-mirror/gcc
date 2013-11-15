/* { dg-do compile } */
/* { dg-options "-O3 -fcilkplus" } */

int *a, *b, *c;
int something;

void foo()
{
  int i, j;

  // Declaration and initialization is allowed.
#pragma simd
  for (int i=0; i < 1000; i++)
    a[i] = b[j];

  // Empty initialization is not allowed.
#pragma simd
  for (; i < 5; ++i)		// { dg-error "expected iteration" }
    a[i] = i;

  // Empty condition is not allowed.
#pragma simd
  for (int i=0; ; ++i)		/* { dg-error "missing controlling" } */
    a[i] = i;

  // Empty increment is not allowed.
#pragma simd
  for (int i=0; i < 1234; )		/* { dg-error "missing increment" } */
    a[i] = i*2;

#pragma simd
  i = 5; /* { dg-error "for statement expected" } */

  // Initialization variables must be either integral or pointer types.
  struct S {
    int i;
  };
#pragma simd
  for (struct S ss = { 0 }; ss.i <= 1000; ++ss.i) /* { dg-error "invalid controlling\|invalid type for iteration\|invalid increment" } */
    a[ss.i] = b[ss.i];

  #pragma simd
  for (float f=0.0; f < 15.0; ++f) /* { dg-error "invalid type" } */
    a[(int)f] = (int) f;

  // Pointers are OK.
  #pragma simd
  for (int *i=c; i < &c[100]; ++i)
    *a = '5';

  // Condition of '==' is not allowed.
#pragma simd
  for (int i=j; i == 5; ++i) /* { dg-error "invalid controlling predicate" } */
    a[i] = b[i];

  // The LHS or RHS of the condition must be the initialization variable.
#pragma simd
  for (int i=0; i+j < 1234; ++i) /* { dg-error "invalid controlling predicate" } */
    a[i] = b[i];  

  // Likewise.
#pragma simd
  for (int i=0; 1234 < i + j; ++i) /* { dg-error "invalid controlling predicate" } */
    a[i] = b[i];  

  // Likewise, this is ok.
#pragma simd
  for (int i=0; 1234 + j < i; ++i)
    a[i] = b[i];

  // According to the CilkPlus forum, casts are not allowed, even if
  // they are no-ops.
#pragma simd
  for (int i=0; (char)i < 1234; ++i) /* { dg-error "invalid controlling predicate" } */
    a[i] = b[i];

#pragma simd
  for (int i=255; i != something; --i)
    a[i] = b[i];

#pragma simd
  for (int i=100; i != 5; i += something)
    a[i] = b[i];

  // Increment must be on the induction variable.
#pragma simd
  for (int i=0; i < 100; j++) /* { dg-error "invalid increment expression" } */
    a[i] = b[i];

  // Likewise.
#pragma simd
  for (int i=0; i < 100; j = i + 1) /* { dg-error "invalid increment expression" } */
    a[i] = b[i];

  // Likewise.
#pragma simd
  for (int i=0; i < 100; i = j + 1) /* { dg-error "invalid increment expression" } */
    a[i] = b[i];

#pragma simd
  for (int i=0; i < 100; i = i + 5)
    a[i] = b[i];

  // Only PLUS and MINUS increments are allowed.
#pragma simd
  for (int i=0; i < 100; i *= 5) /* { dg-error "invalid increment expression" } */
    a[i] = b[i];

#pragma simd
  for (int i=0; i < 100; i -= j)
    a[i] = b[i];

#pragma simd
  for (int i=0; i < 100; i = i + j)
    a[i] = b[i];

#pragma simd
  for (int i=0; i < 100; i = j + i)
    a[i] = b[i];

#pragma simd
  for (int i=0; i < 100; ++i, ++j) /* { dg-error "invalid increment expression" } */
    a[i] = b[i];

#pragma simd
  for (int *point=0; point < b; ++point)
    *point = 555;

#pragma simd
  for (int *point=0; point > b; --point)
    *point = 555;
}
