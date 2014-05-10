/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wpedantic" } */
/* { dg-prune-output ".*near initialization for.*" } */

typedef unsigned vec __attribute__ ((vector_size (2 * sizeof (int))));
union u { int a; double d; };
struct S { int i; char fam[]; };

int i;
vec v = { 1, 2, 3 }; /* { dg-warning "17:excess elements in vector initializer" } */
int a1 = { 1, 2 }; /* { dg-warning "15:excess elements in scalar initializer" } */
int a2[2] = { 1, 2, 3 }; /* { dg-warning "21:excess elements in array initializer" } */
int a3[] = { [1 ? 1 : i] = 0 }; /* { dg-warning "15:array index in initializer is not an integer constant expression" } */
int a4[] = { [1 ... 1 ? 2 : i] = 0 }; /* { dg-warning "15:array index in initializer is not an integer constant expression" } */
char a5[] = ("lol"); /* { dg-warning "13:array initialized from parenthesized string constant" } */
char a6[] = { ("foo") }; /* { dg-warning "13:array initialized from parenthesized string constant" } */
char *a7 = (char []) { ("bar") }; /* { dg-warning "12:array initialized from parenthesized string constant" } */
union u u = { 1, 1.0 }; /* { dg-warning "18:excess elements in union initializer" } */
struct S s = { 1, 2 }; /* { dg-warning "14:initialization of a flexible array member" } */
