/*
TEST_OUTPUT:
---
fail_compilation/fail17570.d(12): Error: cannot use function constraints for non-template functions. Use `static if` instead
fail_compilation/fail17570.d(13): Error: declaration expected, not `{`
fail_compilation/fail17570.d(15): Error: `}` expected following members in `struct` declaration
fail_compilation/fail17570.d(11):        struct `S` starts here
---
*/

struct S(T) {
    void func() if(isIntegral!T)
    {}
}
