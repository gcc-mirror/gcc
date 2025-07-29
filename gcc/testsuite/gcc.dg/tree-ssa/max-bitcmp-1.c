/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop -fdump-tree-optimized" } */

/* PR tree-optimization/95906 */
/* this should become MAX_EXPR<a,b> */

int f2(int a, int b)
{
    int cmp = -(a > b);
    return (cmp & a) | (~cmp & b);
}

/*  we should not end up with -_2 */
/*  we should not end up and & nor a `+ -1` */
/* In optimized we should have a max.  */
/* { dg-final { scan-tree-dump-not " -\[a-zA-Z_\]" "forwprop1" } } */
/* { dg-final { scan-tree-dump-not " & " "forwprop1" } } */
/* { dg-final { scan-tree-dump-not " . -1" "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR " 1 "optimized" } } */
