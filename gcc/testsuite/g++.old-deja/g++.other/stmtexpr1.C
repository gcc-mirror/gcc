// { dg-do assemble  }
// { dg-options "-O" }
// Origin: Thomas Kunert <kunert@physik.tu-dresden.de>

#include <ctype.h>
 
bool  f( char c )
{
    return tolower( c );
}
