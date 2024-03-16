/* Test compatibility of unprototyped and prototyped function types (C23 made
   the case of types affected by default argument promotions compatible, before
   removing unprototyped functions completely).  Test always-invalid-in-C23
   usages, in C11 mode.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

void f1 (); /* { dg-message "previous declaration" } */
void f1 (int, ...); /* { dg-error "conflicting types" } */
/* { dg-message "ellipsis" "" { target *-*-* } .-1 } */

void f2 (int, ...); /* { dg-message "previous declaration" } */
void f2 (); /* { dg-error "conflicting types" } */
/* { dg-message "ellipsis" "" { target *-*-* } .-1 } */

void f3 (); /* { dg-message "previous declaration" } */
void f3 (char, ...); /* { dg-error "conflicting types" } */
/* { dg-message "ellipsis" "" { target *-*-* } .-1 } */

void f4 (char, ...); /* { dg-message "previous declaration" } */
void f4 (); /* { dg-error "conflicting types" } */
/* { dg-message "ellipsis" "" { target *-*-* } .-1 } */
