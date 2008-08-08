/* { dg-do compile } */
/* { dg-options "" } */
/* test redeclarations with void and implicit int */
extern foo1(); /* { dg-message "note: previous declaration" } */
extern void foo1(); /* { dg-error "conflicting types" } */

extern void foo2(); /* { dg-message "note: previous declaration" } */
extern foo2(); /* { dg-error "conflicting types" } */

void foo3() {} /* { dg-message "note: previous definition" } */
extern foo3(); /* { dg-error "conflicting types" } */

extern foo4(); /* { dg-message "note: previous declaration" } */
void foo4() {} /* { dg-error "conflicting types" } */

extern void foo5(); /* { dg-message "note: previous declaration" } */
foo5() {} /* { dg-warning "conflicting types" } */

foo6() {} /* { dg-message "note: previous definition" } */
extern void foo6(); /* { dg-error "conflicting types" } */

foo7() {} /* { dg-message "note: previous definition" } */
void foo7() {} /* { dg-error "conflicting types" } */

void foo8() {} /* { dg-message "note: previous definition" } */
foo8() {} /* { dg-error "conflicting types" } */

int use9() { foo9(); } /* { dg-message "note: previous implicit declaration" } */
extern void foo9(); /* { dg-warning "conflicting types" } */

int use10() { foo10(); } /* { dg-message "note: previous implicit declaration" } */
void foo10() {} /* { dg-warning "conflicting types" } */

