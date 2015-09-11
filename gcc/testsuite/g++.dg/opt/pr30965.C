/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-icf -fdump-tree-optimized" } */

#include <tr1/functional>
#include <algorithm>

extern void assign( long* variable, long v )
{
        std::transform( variable, variable + 1, variable,
                std::tr1::bind( std::plus< long >(), 0L, v ) );
}
extern void assign( long& variable, long v )
{
        std::transform( &variable, &variable + 1, &variable,
                std::tr1::bind( std::plus< long >(), 0L, v ) );
}

/* { dg-final { scan-tree-dump-times ";; Function" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "variable_..D. = v_..D." 2 "optimized" } } */
