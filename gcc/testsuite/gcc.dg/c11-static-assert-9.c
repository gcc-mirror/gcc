/* Test C11 static assertions.  Omitting the string not supported, but
   -Wno-c11-c23-compat disables the -pedantic diagnostic for that.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic -Wno-c11-c23-compat" } */

_Static_assert (1);
