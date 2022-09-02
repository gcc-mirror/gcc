/* Test compatibility of prototyped function types with and without arguments
   (C2x made the case of types affected by default argument promotions
   compatible, before removing unprototyped functions completely).  Test
   affected usages are not accepted for C2x.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

void f1 (); /* { dg-message "previous declaration" } */
void f1 (float); /* { dg-error "conflicting types" } */

void f2 (float); /* { dg-message "previous declaration" } */
void f2 (); /* { dg-error "conflicting types" } */

void f3 (); /* { dg-message "previous declaration" } */
void f3 (char); /* { dg-error "conflicting types" } */

void f4 (char); /* { dg-message "previous declaration" } */
void f4 (); /* { dg-error "conflicting types" } */

/* Built-in function case.  */
float sqrtf (); /* { dg-warning "conflicting types for built-in function" } */
