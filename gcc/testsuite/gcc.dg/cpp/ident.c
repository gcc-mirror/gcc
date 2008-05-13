/* Test #ident.  */
/* { dg-do compile } */
/* { dg-options "-Wno-deprecated" } */ /* shut off -pedantic */

#ident "this is an ident"

int dummy(void) { return 12; }
