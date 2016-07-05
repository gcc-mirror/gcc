/* Test 'q' suffix with -pedantic on __float128 type constants.  */
/* { dg-do compile } */
/* { dg-require-effective-target __float128 } */
/* { dg-options "-pedantic" } */
/* { dg-add-options __float128 } */

__float128 a = 123.456789q; /* { dg-warning "non-standard suffix on floating constant" } */
