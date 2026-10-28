/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int fn6 (int a, int c) {
    return (a ^ c) & (a | c);
}

_Bool fn6_alt (_Bool a, _Bool c) {
    return (a != c) & (a | c);
}

_Bool fn2_alt (_Bool a, _Bool b, _Bool c) {
    return (a ^ c) & (b | c | a);
}

_Bool fn2_alt2 (_Bool a, _Bool b, _Bool c) {
    return (a ^ c) & (b | a | c);
}

_Bool fn2_alt3 (_Bool a, _Bool b, _Bool c) {
    return (a != c) & (b | c | a);
}

_Bool fn2_alt4 (_Bool a, _Bool b, _Bool c) {
    return (a != c) & (b | a | c);
}

/* { dg-final { scan-tree-dump-times " \\\^ " 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times " != " 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\\| " 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " & " 0 "optimized" } } */
