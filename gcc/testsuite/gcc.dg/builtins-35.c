/* Test that nan functions are not built-in in C90 mode.  Bug 14635.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=c89" } */

int nan, nanf, nanl, nans, nansf, nansl;
