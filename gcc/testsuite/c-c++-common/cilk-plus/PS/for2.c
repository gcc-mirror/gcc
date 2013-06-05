/* { dg-do compile } */
/* { dg-options "-O3 -fcilkplus" } */

// Test storage classes in the initialization of a <#pragma simd> for
// loop.

int *a, *b;

void foo()
{
#pragma simd
  for (static int foo=5; foo < 10; ++foo)
    a[foo] = b[foo];
  /* { dg-error "declaration of static variable" "storage class1" { target *-*-* } 12 } */
  /* { dg-error "induction variable cannot be static" "storage class2" { target *-*-* } 12 } */

  static int bar;
#pragma simd
  for (bar=0; bar < 1000; ++bar) /* { dg-error "induction variable cannot be static" } */
    a[bar] = bar;

#pragma simd
  for (extern int var=0; var < 1000; ++var)
    a[var] = var;
  /* { dg-error "has both 'extern' and initializer" "extern" { target *-*-* } 23 } */
  /* { dg-error "declaration of static variable" "" { target *-*-* } 23 } */
  /* { dg-error "induction variable cannot be static" "" { target *-*-* } 23 } */

  extern int extvar;
#pragma simd
  for (extvar = 0; extvar < 1000; ++extvar) /* { dg-error "induction variable cannot be extern" } */
    b[extvar] = a[extvar];

  // This seems like it should be ok.
  // Must check with standards people.
#pragma simd
  for (auto int autoi = 0; autoi < 1000; ++autoi)
    b[autoi] = a[autoi] * 2;
  // Similarly here.
  auto int autoj;
#pragma simd
  for (auto int autoj = 0; autoj < 1000; ++autoj)
    b[autoj] = a[autoj] * 2;

  register int regi;
#pragma simd
  for (regi = 0; regi < 1000; ++regi) /* { dg-error "induction variable cannot be declared register" } */
    b[regi] = a[regi] * 2;

#pragma simd
  for (register int regj = 0; regj < 1000; ++regj) /* { dg-error "induction variable cannot be declared register" } */
    b[regj] = a[regj] * 2;

  volatile int vi;
#pragma simd
  for (vi=0; vi<1000; ++vi) /* { dg-error "induction variable cannot be volatile" } */
    a[vi] = b[vi];

#pragma simd
  for (volatile int vj=0; vj<1000; ++vj) /* { dg-error "induction variable cannot be volatile" } */
    a[vj] = b[vj];

#pragma simd
  for (const int ci=0; ci<1000; ++ci) /* { dg-error "increment of read-only var" } */
    a[ci] = b[ci];
}
