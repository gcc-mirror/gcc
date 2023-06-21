/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-phiprop1-details -fdump-tree-release_ssa" } */
int max(int a, int b)
{
        int *ptr;
        if (a > b)
          ptr = &a;
        else
          ptr = &b;
        return *ptr;
}

/* { dg-final { scan-tree-dump-times "Inserting PHI for result of load" 1 "phiprop1"} } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 1 "release_ssa"} } */
