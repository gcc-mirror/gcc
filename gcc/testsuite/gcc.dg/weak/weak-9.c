/* { dg-do compile } */
/* { dg-options "-fno-common" } */

/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?f1" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?f2" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?f3" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?f4" } } */
/* { dg-final { scan-assembler "notf1" } } */
/* { dg-final { scan-assembler "notf2" } } */
/* { dg-final { scan-assembler "notf3" } } */
/* { dg-final { scan-assembler "notf4" } } */

void f1() __attribute__((weak, alias("notf1")));
void f2() __attribute__((alias("notf2"), weak));

#pragma weak f3=notf3
void f3();

void f4();
#pragma weak f4=notf4
