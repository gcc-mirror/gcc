/* Verify that the limits defined in <stdint.h> are those GCC expects
   internally to be defined and that they are usable in #if
   conditions.  Freestanding version.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -ffreestanding" } */

/* The test is that there are no diagnostics, so just include the
   hosted version.  */
#include "c99-stdint-7.c"
