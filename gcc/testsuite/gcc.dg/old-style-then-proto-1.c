/* Test for old-style definition followed by prototype declaration.
   Mismatched qualifiers used to be wrongly forbidden.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

void f1() {}
void f1(void); /* { dg-warning "prototype for 'f1' follows non-prototype definition" } */

void f2() {} /* { dg-message "note: previous definition of 'f2' was here" } */
void f2(int); /* { dg-error "prototype for 'f2' declares more arguments than previous old-style definition" } */

void f3(a) int a; {} /* { dg-message "note: previous definition of 'f3' was here" } */
void f3(void); /* { dg-error "prototype for 'f3' declares fewer arguments than previous old-style definition" } */

void f4(a) int a; {}
void f4(int); /* { dg-warning "prototype for 'f4' follows non-prototype definition" } */

void f5(a) int a; {} /* { dg-message "note: previous definition of 'f5' was here" } */
void f5(int, int); /* { dg-error "prototype for 'f5' declares more arguments than previous old-style definition" } */

void f6(a) int a; {} /* { dg-message "note: previous definition of 'f6' was here" } */
void f6(int, ...); /* { dg-error "conflicting types for 'f6'" } */

void f7(a, b) int a, b; {} /* { dg-message "note: previous definition of 'f7' was here" } */
void f7(int); /* { dg-error "prototype for 'f7' declares fewer arguments than previous old-style definition" } */

void f8(a, b) int a, b; {} /* { dg-message "note: previous definition of 'f8' was here" } */
void f8(int, ...); /* { dg-error "conflicting types for 'f8'" } */

void f9(a, b) int a, b; {}
void f9(int, int); /* { dg-warning "prototype for 'f9' follows non-prototype definition" } */

void f10(a, b) int a, b; {} /* { dg-message "note: previous definition of 'f10' was here" } */
void f10(int, long); /* { dg-error "prototype for 'f10' declares argument 2 with incompatible type" } */

void f11(a, b) int a, b; {} /* { dg-message "note: previous definition of 'f11' was here" } */
void f11(long, int); /* { dg-error "prototype for 'f11' declares argument 1 with incompatible type" } */

void f12(a, b) const int a; volatile int b; {}
void f12(volatile int, const int); /* { dg-warning "prototype for 'f12' follows non-prototype definition" } */

void f13(a) const int a[2][2]; {} /* { dg-message "note: previous definition of 'f13' was here" } */
void f13(volatile int [2][2]); /* { dg-error "prototype for 'f13' declares argument 1 with incompatible type" } */
