/* TEST_OUTPUT:
---
fail_compilation/failcontracts.d(18): Error: missing `{ ... }` for function literal
fail_compilation/failcontracts.d(18): Error: semicolon expected following auto declaration, not `bode`
fail_compilation/failcontracts.d(19): Error: function declaration without return type
fail_compilation/failcontracts.d(19):        Note that constructors are always named `this`
fail_compilation/failcontracts.d(19): Error: variable name expected after type `test1()`, not `bode`
fail_compilation/failcontracts.d(19): Error: semicolon expected following function declaration, not `bode`
fail_compilation/failcontracts.d(20): Error: semicolon expected following function declaration, not `bode`
fail_compilation/failcontracts.d(22): Error: unexpected `(` in declarator
fail_compilation/failcontracts.d(22): Error: found `T` when expecting `)`
fail_compilation/failcontracts.d(22): Error: expected `{`, not `;` for enum declaration
---
*/

void test()
{
    auto f1 = function() bode;
    auto test1() bode;
    auto test2()() bode;

    enum : int (int function() bode T);
}
