/* PR 7263:  __extension__ keyword doesn't suppress warning on LL or ULL constants.  */
/* { dg-do compile } */
/* { dg-options "-std=c89 -pedantic-errors" } */
#include "pr7263-2.h"
unsigned long long /* { dg-error "ISO C90 does not support .long long." } */
bar ()
{
  return BIG_EXT;
}

unsigned long long /* { dg-error "ISO C90 does not support .long long." } */
bar2 ()
{
  return 0x1b27da572ef3cd86ULL; /* { dg-error "use of C99 long long integer constant" } */
}


unsigned long long /* { dg-error "ISO C90 does not support .long long." } */
bar3 ()
{
  return __extension__ (0x1b27da572ef3cd86ULL);
}

__extension__ unsigned long long 
bar4 ()
{
  return BIG; 
}
