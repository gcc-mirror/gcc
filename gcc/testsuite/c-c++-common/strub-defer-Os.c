/* { dg-do run } */
/* { dg-options "-fstrub=strict -Os" } */
/* { dg-require-effective-target strub } */

/* Check that a strub function called by another strub function defers the
   strubbing to its caller at -Os.  */

#include "strub-defer-O3.c"
