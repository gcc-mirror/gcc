/*
TEST_OUTPUT:
---
fail_compilation/udaparams.d(31): Error: variadic parameter cannot have user-defined attributes
fail_compilation/udaparams.d(32): Error: variadic parameter cannot have user-defined attributes
fail_compilation/udaparams.d(34): Error: user-defined attributes cannot appear as postfixes
fail_compilation/udaparams.d(35): Error: user-defined attributes cannot appear as postfixes
fail_compilation/udaparams.d(36): Error: user-defined attributes cannot appear as postfixes
fail_compilation/udaparams.d(38): Error: `@safe` attribute for function parameter is not supported
fail_compilation/udaparams.d(39): Error: `@safe` attribute for function parameter is not supported
fail_compilation/udaparams.d(40): Error: `@safe` attribute for function parameter is not supported
fail_compilation/udaparams.d(43): Error: `@system` attribute for function parameter is not supported
fail_compilation/udaparams.d(44): Error: `@trusted` attribute for function parameter is not supported
fail_compilation/udaparams.d(45): Error: `@nogc` attribute for function parameter is not supported
fail_compilation/udaparams.d(51): Error: cannot put a storage-class in an `alias` declaration.
fail_compilation/udaparams.d(52): Error: cannot put a storage-class in an `alias` declaration.
fail_compilation/udaparams.d(53): Error: semicolon expected to close `alias` declaration, not `=>`
fail_compilation/udaparams.d(53): Error: declaration expected, not `=>`
fail_compilation/udaparams.d(54): Error: semicolon expected to close `alias` declaration, not `=>`
fail_compilation/udaparams.d(54): Error: declaration expected, not `=>`
fail_compilation/udaparams.d(57): Error: basic type expected, not `@`
fail_compilation/udaparams.d(57): Error: identifier expected for template value parameter
fail_compilation/udaparams.d(57): Error: found `@` when expecting `)`
fail_compilation/udaparams.d(57): Error: basic type expected, not `3`
fail_compilation/udaparams.d(57): Error: found `3` when expecting `)`
fail_compilation/udaparams.d(57): Error: semicolon expected following function declaration
fail_compilation/udaparams.d(57): Error: declaration expected, not `)`
---
*/

void vararg1(int a, @(10) ...);
extern(C) void vararg2(int a, @(10) ...);

void rhsuda(int a @(10));
void rhsuda2(int @(10));
void rhsuda3(int[] arr @(10) ...);

void wrongAttr1(@safe int);
void wrongAttr2(@safe void function());
void wrongAttr3(@safe void delegate());


void test16(A)(A a @system);
void test16(A)(A a @trusted);
void test16(A)(A a @nogc);

// lambdas without parentheses
alias test19a = @safe b => 1 + 2;
alias test19b = @system b => 1 + 2;
alias test19c = @nogc b => 1 + 2;
alias test19d = @(2) @system b => 1 + 2;
alias test19e = @safe @(2) b => 1 + 2;
alias test19f = extern(C++) b => 1 + 2;
alias test19g = align(2) b => 1 + 2;

// UDAs on Template parameter aren't supported
void test21(@(3) T)(T t) {}
