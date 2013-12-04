/* Test for designated initializers: invalid uses of string constants
   should not ICE.  PR 42262.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

int a[] = { [0 ... 1] = "", [0] = "" }; /* { dg-error "initial" } */
