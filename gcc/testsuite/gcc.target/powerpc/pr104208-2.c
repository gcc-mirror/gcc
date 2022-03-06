/* PR target/104208 */
/* { dg-do compile } */
/* { dg-options "-O2 -mlong-double-128 -mabi=ieeelongdouble -mlong-double-64 -mno-vsx -Wno-psabi" } */

/* Verify we do not emit an error with the options above.  IEEE 128-bit
   long double requires VSX, so using -mno-vsx would normally generate
   an error.  However, if we have changed to a 64-bit long double, then
   we should allow it.  */

int i;
