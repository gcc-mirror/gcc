/*
TEST_OUTPUT:
---
fail_compilation/fail222.d(11): Error: template `fail222.getMixin(TArg..., int i = 0)()` template tuple parameter must be last one
fail_compilation/fail222.d(18): Error: template instance `getMixin!()` does not match template declaration `getMixin(TArg..., int i = 0)()`
fail_compilation/fail222.d(21): Error: template instance `fail222.Thing!()` error instantiating
fail_compilation/fail222.d(23): Error: template `fail222.fooBar(A..., B...)()` template tuple parameter must be last one
---
*/

string getMixin(TArg..., int i = 0)()
{
    return ``;
}

class Thing(TArg...)
{
    mixin(getMixin!(TArg)());
}

public Thing!() stuff;

void fooBar (A..., B...)() {}
