/* Test that binary constants are diagnosed in C11 mode: -pedantic.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic" } */

int a = 0b1; /* { dg-warning "binary constants" } */
#if 0b101 /* { dg-warning "binary constants" } */
#endif

int b = 0B1; /* { dg-warning "binary constants" } */
#if 0B101 /* { dg-warning "binary constants" } */
#endif
