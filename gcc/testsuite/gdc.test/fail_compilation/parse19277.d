/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/parse19277.d(13): Deprecation: storage class `ref` has no effect in type aliases
fail_compilation/parse19277.d(14): Deprecation: storage class `__gshared` has no effect in type aliases
fail_compilation/parse19277.d(15): Deprecation: storage class `static` has no effect in type aliases
fail_compilation/parse19277.d(16): Deprecation: storage class `extern` has no effect in type aliases
fail_compilation/parse19277.d(17): Deprecation: storage class `scope` has no effect in type aliases
---
*/

alias T = ref int;
alias U = __gshared int;
alias V = static int;
alias W = extern int;
alias Dg = scope void delegate();

alias F = ref pure nothrow @nogc @safe @live int function();
alias G = ref pure nothrow @nogc @system @live int delegate();
