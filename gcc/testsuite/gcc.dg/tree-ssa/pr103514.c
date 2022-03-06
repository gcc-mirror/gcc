/* { dg-do compile } */
/* { dg-options "-O --param logical-op-non-short-circuit=1 -fdump-tree-optimized" } */
#include <stdbool.h>

bool
i (bool a, bool b)
{
     return (a & b) ^ (a == b);
}

bool
j (bool a, bool b)
{
     return (a & b) == (a ^ b);
}

bool
g (bool a, bool b)
{
    return (a && b) == (a ^ b); 
}

bool
h (bool a, bool b)
{
     return (a && b) ^ (a == b);
}


/* Make sure we have removed "==" and "^" and "&". */
/* { dg-final { scan-tree-dump-not "&" "optimized"} } */
/* { dg-final { scan-tree-dump-not "\\^"  "optimized"} } */
/* { dg-final { scan-tree-dump-not "==" "optimized"} } */
