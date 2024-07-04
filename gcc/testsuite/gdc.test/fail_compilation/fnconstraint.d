/*
TEST_OUTPUT:
---
fail_compilation/fnconstraint.d(14): Error: template constraint must follow parameter lists and attributes
fail_compilation/fnconstraint.d(14): Error: declaration expected, not `if`
fail_compilation/fnconstraint.d(23): Error: template constraint must follow parameter lists and attributes
fail_compilation/fnconstraint.d(23): Error: declaration expected, not `if`
fail_compilation/fnconstraint.d(27): Error: `}` expected following members in `struct` declaration
fail_compilation/fnconstraint.d(19):        struct `S` starts here
---
*/
void foo()()
in(true)
if (true)
{}

alias f = foo!();

struct S
{
    this()()
    if (true)
    if (true) {}
}

S s;
