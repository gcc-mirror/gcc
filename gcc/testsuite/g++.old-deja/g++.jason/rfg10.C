// { dg-do assemble  }
// { dg-options "-pedantic-errors" }
// Bug: g++ doesn't notice the overflow in the enum values.

#include <limits.h>
 
enum COLOR
{
    red,
    green = ULONG_MAX,
    blue		     // { dg-error "too large for .unsigned long" }
};
