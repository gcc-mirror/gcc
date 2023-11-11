/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */
int f(int a)
{
        int b = a & 1;
        int c = b == 0;
        int d = ~a;
        int e = d & 1;
        return c == e;
}

/* This should be optimized to just `return 1` */
/* { dg-final { scan-tree-dump-not " == " "optimized"} } */
/* { dg-final { scan-tree-dump-times "return 1" 1 "optimized"} } */
