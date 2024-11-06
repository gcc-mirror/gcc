/* Test that octal constants are accepted in C2Y mode: compat warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -Wc23-c2y-compat" } */

int a = 0o7; /* { dg-warning "C2Y feature" } */
#if 0o107 /* { dg-warning "C2Y feature" } */
#endif

int b = 0O7; /* { dg-warning "C2Y feature" } */
#if 0O107 /* { dg-warning "C2Y feature" } */
#endif
