/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O -mx32 -mtune=corei7 -maddress-mode=short" } */

extern int foo(int, ...);
int bar(void) {
  long double l = 1.2345E6;
  foo(0, l);
  return 0;
}
