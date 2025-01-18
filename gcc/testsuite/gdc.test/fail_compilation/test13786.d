/*
TEST_OUTPUT:
---
fail_compilation/test13786.d(12): Error: debug `abc` declaration must be at module level
fail_compilation/test13786.d(13): Error: version `abc` declaration must be at module level
fail_compilation/test13786.d(16): Error: template instance `test13786.T!()` error instantiating
---
*/

template T()
{
    debug = abc;
    version = abc;
}

alias X = T!();
