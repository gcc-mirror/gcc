// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized" }

import gcc.attributes;

int func() @cold
{
    return 0;
}

@cold int var = 0; // { dg-warning ".cold. attribute ignored" }

// { dg-final { scan-tree-dump "func\[^\r\n\]*(unlikely executed)" "optimized" } }
