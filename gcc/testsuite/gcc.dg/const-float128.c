/* Test 'q' and 'Q' suffixes on __float128 type constants.  */
/* { dg-do compile } */
/* { dg-require-effective-target __float128 } */
/* { dg-options "" } */
/* { dg-add-options __float128 } */

__float128 a = 123.456789q;
__float128 b = 123.456789Q;
