/* PR 7263:  __extension__ keyword doesn't suppress warning on LL or ULL constants.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */
#include "pr7263-3.h"
__complex__  bar () /* { dg-error "ISO C does not support plain .complex. meaning .double complex." } */
{
  return _Complex_I_ext;
}

__extension__ __complex__ 
bar2 ()
{
  return _Complex_I;
}

__complex__ bar3 () /* { dg-error "ISO C does not support plain .complex. meaning .double complex." } */
{
  return _Complex_I; /* { dg-error "imaginary constants are a GCC extension" } */
}
