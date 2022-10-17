/*
TEST_OUTPUT:
---
fail_compilation/fail17570.d(11): Error: cannot use function constraints for non-template functions. Use `static if` instead
fail_compilation/fail17570.d(11): Error: declaration expected, not `if`
fail_compilation/fail17570.d(14): Error: `}` expected following members in `struct` declaration at fail_compilation/fail17570.d(10)
---
*/

struct S(T) {
    void func() if(isIntegral!T)
    {}
}
