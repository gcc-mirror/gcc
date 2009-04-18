/* Test that "" is not specially interpreted as "<stdin>" in a #line
   directive.  PR 39646.  */

/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

extern int x;

#line 24 ""
extern char z[sizeof __FILE__ == 1];
