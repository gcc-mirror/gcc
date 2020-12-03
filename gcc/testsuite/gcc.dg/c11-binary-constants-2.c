/* Test that binary constants are diagnosed in C11 mode: -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

int a = 0b1; /* { dg-error "binary constants" } */
#if 0b101 /* { dg-error "binary constants" } */
#endif

int b = 0B1; /* { dg-error "binary constants" } */
#if 0B101 /* { dg-error "binary constants" } */
#endif
