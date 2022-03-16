/* PR target/104208 */
/* { dg-do compile } */
/* { dg-options "-O2 -mlong-double-128 -mabi=ibmlongdouble -mlong-double-64 -Wno-psabi" } */

/* Verify we do not emit an error with the options above.  It is allowed
   to reset the long double size to 64-bits after a 128-bit long double
   ABI has been selected.  */

int i;
