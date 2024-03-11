/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1" } */
/* PR tree-optimization/106164 */
/* PR tree-optimization/111456 */

_Bool f(int a)
{
        _Bool t = a == 3;
        unsigned t1 = a;
        _Bool t2 = t1 >= 3;
        return t | t2;
}

/* Should be able to optimize down to just `a > 2` during forwprop1 */
/* { dg-final { scan-tree-dump-not "a_\[0-9\]+.D. == 3" "forwprop1" } } */

_Bool f1(int b)
{
        _Bool t = b == 3;
        short t1 = b;
        _Bool t2 = t1 >= 3;
        return t | t2;
}

/* Should be able to optimize down to just `a > 2` during forwprop1 as `((short)a) >= 3` is
   true already when `a == 3`. */
/* { dg-final { scan-tree-dump-not "b_\[0-9\]+.D. == 3" "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "_\[0-9\]+ > 2" 2 "forwprop1" } } */
