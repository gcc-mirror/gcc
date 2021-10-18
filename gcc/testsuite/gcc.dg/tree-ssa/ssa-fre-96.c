/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

_Bool f1(unsigned x, unsigned y, unsigned *res)
{
    _Bool t = __builtin_add_overflow(x, y, res);
    unsigned res1;
    _Bool t1 = __builtin_add_overflow(x, y, &res1);
    *res -= res1;
    return t==t1;
}

/* { dg-final { scan-tree-dump-times "ADD_OVERFLOW" 1 "fre1" } } */
/* { dg-final { scan-tree-dump "return 1;" "fre1" } } */
