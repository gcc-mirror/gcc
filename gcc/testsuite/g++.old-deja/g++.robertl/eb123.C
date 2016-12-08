// { dg-do assemble  }
// { dg-options "-O2 -W   " }
#include "stdio.h"

void writeNote()
#if __cplusplus <= 201402L
throw( int )			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
{
    printf( "hello world\n" );
    try { }
    catch( int ){ throw; }
}
