/* Test for implicit int: in C90 only.  Function parameters in old-style
   function definition.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

void foo (a) { }
