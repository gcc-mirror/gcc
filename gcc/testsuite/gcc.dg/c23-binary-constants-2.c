/* Test that binary constants are accepted in C23 mode: compat warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -Wc11-c23-compat" } */

int a = 0b1; /* { dg-warning "C23 feature" } */
#if 0b101 /* { dg-warning "C23 feature" } */
#endif

int b = 0B1; /* { dg-warning "C23 feature" } */
#if 0B101 /* { dg-warning "C23 feature" } */
#endif
