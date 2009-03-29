/* Test for constraints on constant expressions.  In C90 it is clear that
   certain constructs are not permitted in unevaluated parts of an
   expression (except in sizeof); in C99 it might fall within implementation
   latitude.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk>; inspired by
   http://deja.com/getdoc.xp?AN=524271595&fmt=text by Peter Seebach.
*/
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

extern int bar (void);

void
foo (void)
{
  int i;
  static int j = (1 ? 0 : (i = 2)); /* { dg-error "initial" "assignment" } */
  static int k = (1 ? 0 : ++i); /* { dg-error "initial" "increment" } */
  static int l = (1 ? 0 : --i); /* { dg-error "initial" "decrement" } */
  static int m = (1 ? 0 : bar ()); /* { dg-error "initial" "function call" } */
  static int n = (1 ? 0 : (2, 3)); /* { dg-error "initial" "comma" } */
}
