/* Test #ident.  */
/* { dg-do compile } */
/* { dg-options "" } */ /* shut off -pedantic */

#ident "this is an ident"

int dummy(void) { return 12; }

/* { dg-final { scan-assembler ident.c "this is an ident" } } */
