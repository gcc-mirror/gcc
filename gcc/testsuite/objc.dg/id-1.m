/* Test the id type warning.  */
/* { dg-do compile } */

typedef int id;

id b; /* { dg-warning "Unexpected type for `id'" } */
