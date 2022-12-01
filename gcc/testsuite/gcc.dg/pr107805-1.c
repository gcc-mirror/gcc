/* { dg-do compile } */
typedef int t;
typedef struct { double a; int b; } t; /* { dg-error "conflicting types" } */
t x; /* No warning here.  */

