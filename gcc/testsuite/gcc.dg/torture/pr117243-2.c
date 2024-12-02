/* { dg-do compile } */
/* { dg-options "-fno-tree-ch -fdump-tree-optimized" } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

/* PR tree-optimization/117243 */
/* PR tree-optimization/116749 */

/* main1 should be an infinite but sometimes it gets optimized incorrectly into
   an __builtin_unreachable(); which is not valid.  */
int main1 (void)
{
    int g=0;
    int l1[1];
    int *l2 = &g;
    int i;
    for (i=0; i<1; i++)
        l1[i] = (1);
    for (g=0; g; ++g)
    {
        int *l3[1] = {&l1[0]};
    }
    *l2 = *l1;
b:
    for (i=0; i<2; ++i)
    { 
        if (i)
            goto b;
        if (g)
            continue;
    }
    return 0;
}

/* { dg-final { scan-tree-dump-not "__builtin_unreachable " "optimized"} } */
