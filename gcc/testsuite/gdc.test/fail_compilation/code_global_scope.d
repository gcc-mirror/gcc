/**
TEST_OUTPUT:
---
fail_compilation/code_global_scope.d(18): Error: `switch` statement must be inside function scope
fail_compilation/code_global_scope.d(19): Error: `do` statement must be inside function scope
fail_compilation/code_global_scope.d(20): Error: `foreach` statement must be inside function scope
fail_compilation/code_global_scope.d(21): Error: `while` statement must be inside function scope
fail_compilation/code_global_scope.d(22): Error: `if` statement must be inside function scope
fail_compilation/code_global_scope.d(23): Error: `return` statement must be inside function scope
fail_compilation/code_global_scope.d(24): Error: `goto` statement must be inside function scope
fail_compilation/code_global_scope.d(25): Error: `continue` statement must be inside function scope
fail_compilation/code_global_scope.d(26): Error: `break` statement must be inside function scope
---
*/



switch s;
do d;
foreach (i; 0 .. 4) {}
while (x) {}
if (y) {}
return 0;
goto A;
continue B;
break;
