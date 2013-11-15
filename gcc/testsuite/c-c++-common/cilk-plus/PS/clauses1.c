/* { dg-do compile } */
/* { dg-options "-O3 -Werror -Wunknown-pragmas -fcilkplus" } */

volatile int *a, *b;

void foo()
{
  int i, j, k;

#pragma simd assert /* { dg-error "expected '#pragma simd' clause" } */
  for (i=0; i < 100; ++i)
    a[i] = b[i];

#pragma simd vectorlength /* { dg-error "expected '\\('" } */
  for (int i=0; i < 1000; ++i)
    a[i] = b[j];

#pragma simd vectorlength /* { dg-error "expected '\\('" } */
  for (int i=0; i < 1000; ++i)
    a[i] = b[j];

#pragma simd vectorlength(sizeof (a) == sizeof (float) ? 4 : 8)
  for (int i=0; i < 1000; ++i)
    a[i] = b[j];

#pragma simd vectorlength(4,8) /* { dg-error "expected '\\)'" } */
  for (int i=0; i < 1000; ++i)
    a[i] = b[j];

#pragma simd vectorlength(i) /* { dg-error "\(vectorlength must be an integer\|in a constant\)" } */
  for (int i=0; i < 1000; ++i)
    a[i] = b[j];

#pragma simd linear(35) /* { dg-error "expected identifier" } */
  for (int i=0; i < 1000; ++i)
    a[i] = b[j];

#pragma simd linear(blah) /* { dg-error "'blah' \(undeclared\|has not been\)" } */
  for (int i=0; i < 1000; ++i)
    a[i] = b[j];

#pragma simd linear(j, 36, k) /* { dg-error "expected" } */
  for (int i=0; i < 1000; ++i)
    a[i] = b[j];

#pragma simd linear(i, j)
  for (int i=0; i < 1000; ++i)
    a[i] = b[j];

#pragma simd linear(i)
  for (int i=0; i < 1000; ++i)
    a[i] = b[j];

#pragma simd linear(i : 4)
  for (int i=0; i < 1000; ++i)
    a[i] = b[j];

#pragma simd linear(i : 2, j : 4, k)
  for (int i=0; i < 1000; ++i)
    a[i] = b[j];

#pragma simd linear(j : sizeof (a) == sizeof (float) ? 4 : 8)
  for (int i=0; i < 1000; ++i)
    a[i] = b[j];

  // And now everyone in unison!
#pragma simd linear(j : 4) vectorlength(4)
  for (int i=0; i < 1000; ++i)
    a[i] = b[j];

#pragma simd linear(blah2, 36)
  /* { dg-error "'blah2' \(undeclared\|has not been\)" "undeclared" { target *-*-* } 71 } */
  /* { dg-error "expected" "expected" { target *-*-* } 71 } */
  for (int i=0; i < 1000; ++i)
    a[i] = b[j];

#pragma simd linear(j : k)
  for (int i=0; i < 1234; ++i)
    a[i] = b[j];
}
