/* Test attempt to redefine 'id' in an incompatible fashion.  */
/* { dg-do compile } */

typedef int id;  /* { dg-error "conflicting types for .id." } */
/* { dg-message "previous declaration of .id." "" { target *-*-* } 0 } */

id b;
