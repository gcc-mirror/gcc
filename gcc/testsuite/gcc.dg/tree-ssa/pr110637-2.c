/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */
int f(int a)
{
        int b = a & 1;
        int c = b == 0;
        return c;
}

/* This should be optimized to just return `(a&1) ^ 1` or `(~a) & 1`. */
/* { dg-final { scan-tree-dump-not " == " "optimized"} } */
/* { dg-final { scan-tree-dump-times "~a" 1 "optimized"} } */
/* { dg-final { scan-tree-dump-times " & 1" 1 "optimized"} } */
