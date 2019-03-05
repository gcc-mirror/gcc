/*
TEST_OUTPUT:
---
fail_compilation/fail222.d(10): Error: template fail222.getMixin(TArg..., int i = 0)() template tuple parameter must be last one
fail_compilation/fail222.d(17): Error: template instance getMixin!() does not match template declaration getMixin(TArg..., int i = 0)()
fail_compilation/fail222.d(20): Error: template instance fail222.Thing!() error instantiating
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
