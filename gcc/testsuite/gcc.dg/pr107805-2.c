/* { dg-do compile } */
typedef int t;
typedef struct { double a; int b; } t; /* { dg-error "conflicting types" } */
t char x; /* { dg-error "two or more data types" } */
