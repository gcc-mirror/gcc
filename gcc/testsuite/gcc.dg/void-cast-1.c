/* Don't warn where the left-hand side of a comma expression is a
   comma expression whose right-hand side is cast to void.  Bug
   21159.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

int a, b, c, d;
int e(void) { return (void)a, b; }
int f(void) { return (void)a, (void)b, c; }
int g(void) { return (void)a, (void)b, (void)c, d; }
