/* { dg-do compile }  */
/* { dg-options "-O2 -fdump-tree-optimized" } */


__attribute__((noipa))
int x(_Bool iftmp, unsigned _6)
{
        return ((-iftmp) & _6) & (~_6);
}
__attribute__((noipa))
int y(_Bool iftmp, unsigned _6)
{
        return (iftmp * _6) & (~_6);
}
__attribute__((noipa))
int z(_Bool iftmp, unsigned _6)
{
        unsigned t = ~_6;
        unsigned t1 = (iftmp ? _6 : 0);
        return t1 & t;
}

/* In this case AND should be removed.  */
/* { dg-final { scan-tree-dump-not " & " "optimized" } } */
