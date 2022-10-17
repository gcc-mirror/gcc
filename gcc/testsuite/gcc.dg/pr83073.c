/* { dg-do compile} */
/* { dg-options "-O2 -fdump-tree-evrp-details -fno-tree-fre -fno-tree-ccp -fno-tree-forwprop" } */

int f(int x)
{
    x = x|1;
    return x & 1;
}

/* { dg-final { scan-tree-dump "gimple_simplified to.* = 1" "evrp" } }  */
