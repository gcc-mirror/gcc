/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */
int f(int a)
{
        int b = a & 1;
        int c = b == 0;
        return c == b;
}

/* This should be optimized to just return 0; */
/* { dg-final { scan-tree-dump-not " == " "optimized"} } */
/* { dg-final { scan-tree-dump "return 0;" "optimized"} } */
