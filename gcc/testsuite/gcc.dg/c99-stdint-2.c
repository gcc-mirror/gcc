/* Verify that <stdint.h> is present and follows the C99 requirements.
   Freestanding version.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors -ffreestanding" } */
/* { dg-require-effective-target ptr32plus } */
/* The test is that there are no diagnostics, so just include the
   hosted version.  */
#include "c99-stdint-1.c"
