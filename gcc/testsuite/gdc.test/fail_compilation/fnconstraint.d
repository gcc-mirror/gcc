/*
TEST_OUTPUT:
---
fail_compilation/fnconstraint.d(13): Error: template constraint must follow parameter lists and attributes
fail_compilation/fnconstraint.d(13): Error: declaration expected, not `if`
fail_compilation/fnconstraint.d(22): Error: template constraint must follow parameter lists and attributes
fail_compilation/fnconstraint.d(22): Error: declaration expected, not `if`
fail_compilation/fnconstraint.d(26): Error: `}` expected following members in `struct` declaration at fail_compilation/fnconstraint.d(18)
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
