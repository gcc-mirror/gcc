/* Test attempt to redefine 'id' in an incompatible fashion.  */
/* { dg-do compile } */

typedef int id;  /* { dg-error "conflicting types for .id." } */
/* { dg-error "previous declaration of .id. was here" "" { target *-*-* } 0 } */

id b;
