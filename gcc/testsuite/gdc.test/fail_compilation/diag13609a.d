/*
TEST_OUTPUT:
---
fail_compilation/diag13609a.d(16): Error: `}` expected following members in `struct` declaration
fail_compilation/diag13609a.d(15):        struct starts here
fail_compilation/diag13609a.d(16): Error: `}` expected following members in `class` declaration
fail_compilation/diag13609a.d(11):        class `C` starts here
---
*/

class C
{
    void foo() {}

    struct {
