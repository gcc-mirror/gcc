/* { dg-do compile } */
/* { dg-options "-fstrub=all" } */

#include "strub-inlinable1.c"

/* With -fstrub=all, the caller becomes a strub context, so the strub-inlinable
   callee is not rejected.  */
