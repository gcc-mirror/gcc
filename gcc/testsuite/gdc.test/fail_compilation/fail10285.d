/*
TEST_OUTPUT:
---
fail_compilation/fail10285.d(13): Error: no identifier for declarator `int`
fail_compilation/fail10285.d(14): Error: expected `,` or `=` after identifier, not `y`
fail_compilation/fail10285.d(15): Error: expected identifier after type, not `bool`
fail_compilation/fail10285.d(16): Error: expected identifier after type, not `int`
fail_compilation/fail10285.d(18): Error: initializer required after `z` when type is specified
---
*/
enum
{
    int = 5,
    int x y,
    int bool i = 3,
    j int k = 3,
    int z
}
