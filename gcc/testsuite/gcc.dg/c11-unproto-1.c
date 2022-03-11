/* Test compatibility of unprototyped and prototyped function types (C2x made
   the case of types affected by default argument promotions compatible, before
   removing unprototyped functions completely).  Test affected usages are not
   accepted for C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

void f1 (); /* { dg-message "previous declaration" } */
void f1 (float); /* { dg-error "conflicting types" } */
/* { dg-message "default promotion" "" { target *-*-* } .-1 } */

void f2 (float); /* { dg-message "previous declaration" } */
void f2 (); /* { dg-error "conflicting types" } */
/* { dg-message "default promotion" "" { target *-*-* } .-1 } */

void f3 (); /* { dg-message "previous declaration" } */
void f3 (char); /* { dg-error "conflicting types" } */
/* { dg-message "default promotion" "" { target *-*-* } .-1 } */

void f4 (char); /* { dg-message "previous declaration" } */
void f4 (); /* { dg-error "conflicting types" } */
/* { dg-message "default promotion" "" { target *-*-* } .-1 } */

/* Built-in function case.  */
float sqrtf (); /* { dg-warning "conflicting types for built-in function" } */
