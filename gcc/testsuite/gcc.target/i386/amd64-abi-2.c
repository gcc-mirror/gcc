/* PR target/26223 */
/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mno-80387" } */
long double foo(long double x) { return x; } /* { dg-error "x87 disabled" } */
long double bar(long double x) { return x; }

