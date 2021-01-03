/* Test that binary constants are accepted in C2X mode: compat warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -Wc11-c2x-compat" } */

int a = 0b1; /* { dg-warning "C2X feature" } */
#if 0b101 /* { dg-warning "C2X feature" } */
#endif

int b = 0B1; /* { dg-warning "C2X feature" } */
#if 0B101 /* { dg-warning "C2X feature" } */
#endif
