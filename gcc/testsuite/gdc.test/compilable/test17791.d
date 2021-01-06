/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
---
*/
deprecated("A deprecated class") {
class DepClass
{
}
}

class NewClass
{
}

void main()
{
    // test that a symbol (which is not likely to be deprecated)
    // is not depercated
    static assert(!__traits(isDeprecated, int));
    // check that a class marked deprecated "isDeprecated"
    static assert(__traits(isDeprecated, DepClass));
    // check that a class not marked deprecated is not deprecated
    static assert(!__traits(isDeprecated, NewClass));
    // Check for expressions (18617)
    static assert(__traits(isDeprecated, { scope foo = new DepClass; }));
}
