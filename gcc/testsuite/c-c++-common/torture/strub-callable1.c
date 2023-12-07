/* { dg-do compile } */
/* { dg-options "-fstrub=strict" } */
/* { dg-require-effective-target strub } */

/* Check that strub and non-strub functions can be called from non-strub
   contexts, and that strub and callable functions can be called from strub
   contexts.  */

#define OMIT_IMPERMISSIBLE_CALLS 1
#include "strub-callable2.c"
