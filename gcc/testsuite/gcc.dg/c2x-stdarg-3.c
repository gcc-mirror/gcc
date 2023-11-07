/* Test C23 variadic functions with no named parameters.  Compilation tests,
   invalid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int f (...); /* { dg-message "previous declaration" } */
int f (); /* { dg-error "conflicting types" } */

int f2 (...); /* { dg-message "previous declaration" } */
int f2 (int); /* { dg-error "conflicting types" } */

int g (); /* { dg-message "previous declaration" } */
int g (...); /* { dg-error "conflicting types" } */

int g2 (int); /* { dg-message "previous declaration" } */
int g2 (...); /* { dg-error "conflicting types" } */
