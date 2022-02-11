/*
TEST_OUTPUT:
---
fail_compilation/ice19950.d(8): Error: undefined identifier `NotHere`
fail_compilation/ice19950.d(9): Error: template instance `ice19950.baz!()` does not match template declaration `baz()(Foo)`
---
*/
alias Foo = NotHere;
alias Bar = baz!();

void baz()(Foo)
    if (true)
{}
