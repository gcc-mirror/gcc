/+ TEST_OUTPUT:
---
fail_compilation/fail21243.d(16): Error: found `(` when expecting `ref` and function literal following `auto`
fail_compilation/fail21243.d(16): Error: semicolon expected following auto declaration, not `int`
fail_compilation/fail21243.d(16): Error: semicolon needed to end declaration of `x` instead of `)`
fail_compilation/fail21243.d(16): Error: declaration expected, not `)`
fail_compilation/fail21243.d(17): Error: `auto` can only be used as part of `auto ref` for function literal return values
fail_compilation/fail21243.d(18): Error: basic type expected, not `(`
fail_compilation/fail21243.d(18): Error: function declaration without return type. (Note that constructors are always named `this`)
fail_compilation/fail21243.d(18): Deprecation: storage class `auto` has no effect in type aliases
fail_compilation/fail21243.d(18): Error: semicolon expected to close `alias` declaration, not `=>`
fail_compilation/fail21243.d(18): Error: declaration expected, not `=>`
fail_compilation/fail21243.d(19): Error: `auto` can only be used as part of `auto ref` for function literal return values
---
+/
auto a = auto (int x) => x;
auto b = function auto (int x) { return x; };
alias c = auto (int x) => x;
alias d = function auto (int x) { return x; };
