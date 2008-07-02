/* Test 'q' suffix with -pedantic on __float128 type constants.  */
/* { dg-do compile { target ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-pedantic" } */

__float128 a = 123.456789q; /* { dg-warning "non-standard suffix on floating constant" } */
