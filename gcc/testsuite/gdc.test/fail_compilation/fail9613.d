// PREMUTE_ARGS:
/*
TEST_OUTPUT:
---
fail_compilation/fail9613.d(12): Error: `(arguments)` expected following `const(byte)`
fail_compilation/fail9613.d(12): Error: semicolon expected following auto declaration, not `.`
---
*/

void main()
{
    auto x = const byte.init;
}
