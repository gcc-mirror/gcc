// Special g++ Options: -O2 -W   
// Build don't link: 
#include "stdio.h"

void writeNote() throw( int )
{
    printf( "hello world\n" );
    try { }
    catch( int ){ throw; }
}
