/* Test silencing the numeric constant extension pedwarns.  */
/* { dg-options "-pedantic-errors" } */
/* { dg-additional-options "-std=gnu17" { target c } } */
/* { dg-additional-options -fext-numeric-literals { target c++14 } } */

#pragma GCC diagnostic push

void f()
{
  2.0j;				/* { dg-error "imaginary" } */
  1.0dd;			/* { dg-error "decimal float" } */
  1.0d;				/* { dg-error "double constant" } */
  0b0100;	   /* { dg-error "binary constant" "" { target { ! c++14 } } } */
#pragma GCC diagnostic ignored "-Wpedantic"
  2.0j;
  1.0dd; /* { dg-error "decimal floating-point" "" { target { ! dfp } } } */
  1.0d;

#ifdef __cplusplus
#pragma GCC diagnostic ignored "-Wc++14-extensions"
#endif
  0b0100;

  1.0K; /* { dg-bogus {fixed-point types not supported in C\+\+} "" { xfail c++ } } */
	/* { dg-error {fixed-point types not supported for this target} "" { target { c && { ! fixed_point } } } .-1 } */
}
