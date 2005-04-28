/* Test further cases of warnings for comma expressions, with and
   without casts to void.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wall" } */
int a, b, c, d;
int e(void) { return (char)a, b; } /* { dg-warning "warning: left-hand operand of comma expression has no effect" } */
int f(void) { return (a ? (void)b : (void)c), d; } /* { dg-warning "warning: left-hand operand of comma expression has no effect" } */
