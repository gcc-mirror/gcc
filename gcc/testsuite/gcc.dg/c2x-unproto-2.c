/* Test compatibility of prototyped function types without arguments and with
   variable arguments (C2x made the case of types affected by default argument
   promotions compatible, before removing unprototyped functions completely).
   Test always-invalid-in-C2x usages, in C2X mode.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

void f1 (); /* { dg-message "previous declaration" } */
void f1 (int, ...); /* { dg-error "conflicting types" } */

void f2 (int, ...); /* { dg-message "previous declaration" } */
void f2 (); /* { dg-error "conflicting types" } */

void f3 (); /* { dg-message "previous declaration" } */
void f3 (char, ...); /* { dg-error "conflicting types" } */

void f4 (char, ...); /* { dg-message "previous declaration" } */
void f4 (); /* { dg-error "conflicting types" } */
