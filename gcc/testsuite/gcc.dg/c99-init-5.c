/* Test for designated initializers: string constants used with
   designator in character array should not initialize the array as a
   whole.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

char g[] = { [7] = "abcd" }; /* { dg-error "initial" } */
char h[10][10] = { [1][1] = "abcd" }; /* { dg-error "initial" } */
char i[10][10] = { [1] = "abcd" };
