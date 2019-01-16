/*
TEST_OUTPUT:
---
fail_compilation/test13786.d(14): Error: debug 123 level declaration must be at module level
fail_compilation/test13786.d(15): Error: debug abc declaration must be at module level
fail_compilation/test13786.d(16): Error: version 123 level declaration must be at module level
fail_compilation/test13786.d(17): Error: version abc declaration must be at module level
fail_compilation/test13786.d(20): Error: template instance test13786.T!() error instantiating
---
*/

template T()
{
    debug = 123;
    debug = abc;
    version = 123;
    version = abc;
}

alias X = T!();
