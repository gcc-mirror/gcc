// { dg-do compile }
// { dg-options "-fdump-tree-optimized" }

import gcc.attributes;

int func(@restrict int ignored) // { dg-warning ".restrict. attribute ignored" }
{
    return 0;
}

int func(@restrict int *parm)
{
    return 0;
}

@restrict int var = 0; // { dg-warning ".restrict. attribute ignored" }

// { dg-final { scan-tree-dump "restrict parm" "optimized" } }
