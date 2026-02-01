/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/opApply_return.d(18): Deprecation: cannot return non-zero compile-time value from `opApply`
fail_compilation/opApply_return.d(18):        Any non-zero value must be the result of calling its delegate
fail_compilation/opApply_return.d(19): Deprecation: cannot return non-zero compile-time value from `opApply`
fail_compilation/opApply_return.d(19):        Any non-zero value must be the result of calling its delegate
fail_compilation/opApply_return.d(20): Deprecation: cannot return non-zero compile-time value from `opApply`
fail_compilation/opApply_return.d(20):        Any non-zero value must be the result of calling its delegate
---
*/

class Tree {
    Tree lhs;
    Tree rhs;
    int opApply(int delegate(Tree) dg) {
        if (lhs && lhs.opApply(dg)) return 1;
        if (dg(this)) return 1;
        if (rhs && rhs.opApply(dg)) return 1;
        return 0;
    }
}
