/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

void unlikely();
void likely();

inline int expect_false(int b) {
    return __builtin_expect(b, 0);
}

void inline_func_hint(int b) {
    if (expect_false(b)) {
        unlikely();
    } else {
        likely();
    }
}

/* { dg-final { scan-tree-dump "_builtin_expect heuristics of edge" "profile_estimate"} } */
