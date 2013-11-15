/* { dg-do compile } */
/* { dg-options "-O3 -fcilkplus" } */

// Test storage classes in the initialization of a <#pragma simd> for
// loop.

int *a, *b;

void foo()
{
#pragma simd
  for (static int tt=5; tt < 10; ++tt) /* { dg-error "before 'static'\|not declared\|expected" } */
    a[tt] = b[tt];

#pragma simd
  for (extern int var=0; var < 1000; ++var) /* { dg-error "before 'extern'\|not declared\|expected" } */
    a[var] = var;

#pragma simd
  for (register int regj = 0; regj < 1000; ++regj) /* { dg-error "before 'register'\|not declared\|expected" } */
    b[regj] = a[regj] * 2;

#pragma simd
  for (volatile int vj=0; vj<1000; ++vj) /* { dg-error "iteration variable cannot be volatile" } */
    a[vj] = b[vj];
}
