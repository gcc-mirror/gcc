/* { dg-do compile } */
/* { dg-require-weak "" } */
/* { dg-require-alias "" } */
/* { dg-options "-fno-common" } */

/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?f1" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?f2" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?f3" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?f4" } } */

void notf1() { }
void notf2() { }
void notf3() { }
void notf4() { }

void f1() __attribute__((weak, alias("notf1")));
void f2() __attribute__((alias("notf2"), weak));

#pragma weak f3=notf3
void f3();

void f4();
#pragma weak f4=notf4
