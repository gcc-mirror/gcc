/* { dg-do compile } */
/* { dg-options "-fshow-column -std=gnu89" } */
/* test redeclarations with void and implicit int */
extern foo1(); /* { dg-message "8:note: previous declaration" } */
extern void foo1(); /* { dg-error "13:conflicting types" } */

extern void foo2(); /* { dg-message "13:note: previous declaration" } */
extern foo2(); /* { dg-error "8:conflicting types" } */

void foo3() {} /* { dg-message "6:note: previous definition" } */
extern foo3(); /* { dg-error "8:conflicting types" } */

extern foo4(); /* { dg-message "8:note: previous declaration" } */
void foo4() {} /* { dg-error "6:conflicting types" } */

extern void foo5(); /* { dg-message "13:note: previous declaration" } */
foo5() {} /* { dg-warning "1:conflicting types" } */

foo6() {} /* { dg-message "1:note: previous definition" } */
extern void foo6(); /* { dg-error "13:conflicting types" } */

foo7() {} /* { dg-message "1:note: previous definition" } */
void foo7() {} /* { dg-error "6:conflicting types" } */

void foo8() {} /* { dg-message "6:note: previous definition" } */
foo8() {} /* { dg-error "1:conflicting types" } */

int use9() { foo9(); } /* { dg-message "note: previous implicit declaration" } */
extern void foo9(); /* { dg-warning "13:conflicting types" } */

int use10() { foo10(); } /* { dg-message "note: previous implicit declaration" } */
void foo10() {} /* { dg-warning "6:conflicting types" } */

