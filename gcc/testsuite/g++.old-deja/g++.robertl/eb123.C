// { dg-do assemble  }
// { dg-options "-O2 -W   " }
#include "stdio.h"

void writeNote() throw( int )
{
    printf( "hello world\n" );
    try { }
    catch( int ){ throw; }
}
