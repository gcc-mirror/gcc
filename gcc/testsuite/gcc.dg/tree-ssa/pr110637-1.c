/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */
int f(int a)
{
        int b = (a & 1)!=0;
        return b;
}

/* This should be optimized to just return (a & 1); */
/* { dg-final { scan-tree-dump-not " == " "optimized"} } */
