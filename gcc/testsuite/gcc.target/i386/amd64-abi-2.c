/* PR target/26223 */
/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-mno-80387" } */
/* { dg-additional-options "-mabi=sysv" { target *-*-mingw* } } */

long double foo(long double x) { return x; } /* { dg-error "x87 disabled" } */
long double bar(long double x) { return x; }

