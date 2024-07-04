/+ TEST_OUTPUT:
---
fail_compilation/fail21243.d(12): Error: found `(` when expecting `ref` and function literal following `auto`
fail_compilation/fail21243.d(12): Error: semicolon expected following auto declaration, not `int`
fail_compilation/fail21243.d(12): Error: semicolon needed to end declaration of `x` instead of `)`
fail_compilation/fail21243.d(12): Error: declaration expected, not `)`
fail_compilation/fail21243.d(13): Error: `auto` can only be used as part of `auto ref` for function literal return values
fail_compilation/fail21243.d(14): Error: `auto` can only be used as part of `auto ref` for function literal return values
fail_compilation/fail21243.d(15): Error: `auto` can only be used as part of `auto ref` for function literal return values
---
+/
auto a = auto (int x) => x;
auto b = function auto (int x) { return x; };
alias c = auto (int x) => x;
alias d = function auto (int x) { return x; };
