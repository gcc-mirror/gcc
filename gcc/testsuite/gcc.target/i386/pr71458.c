/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -fsanitize=bounds" } */
/* { dg-error "-fcheck-pointer-bounds is not supported with Undefined Behavior Sanitizer" "" { target *-*-* } 0 } */

enum {} a[0];
void fn1(int);
void fn2() { fn1(a[-1]); }
