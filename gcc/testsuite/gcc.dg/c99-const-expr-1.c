/* Test for constraints on constant expressions.  In C90 it is clear that
   certain constructs are not permitted in unevaluated parts of an
   expression (except in sizeof); in C99 it might fall within implementation
   latitude; and if the operands are suitable, diagnostics should not be
   issued.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk>; inspired by
   http://deja.com/getdoc.xp?AN=524271595&fmt=text by Peter Seebach.
*/
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

/* The comma operator is in a subexpression that is not evaluated, so OK
   by C99.  In C90 a diagnostic is required since it is not in a sizeof.
*/
int i = (1 ? 0 : (2, 3));
