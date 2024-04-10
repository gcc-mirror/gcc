/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop3-raw -w -Wno-psabi" } */

typedef signed char v16i8 __attribute__((vector_size(16)));
v16i8 f(v16i8 a, v16i8 b)
{
    v16i8 cmp = (a > b);
    return (cmp & a) | (~cmp & b);
}

/* { dg-final { scan-tree-dump-not "bit_(and|ior)_expr" "forwprop3" } } */
/* { dg-final { scan-tree-dump-times {(?n)(?:max_expr|vec_cond_expr)} 1 "forwprop3" } } */
