/* TEST_OUTPUT:
---
fail_compilation/test4946.d(13): Error: 'pure' cannot be placed after a template constraint
fail_compilation/test4946.d(14): Error: 'const' cannot be placed after a template constraint
fail_compilation/test4946.d(15): Error: 'immutable' cannot be placed after a template constraint
fail_compilation/test4946.d(16): Error: 'inout' cannot be placed after a template constraint
fail_compilation/test4946.d(17): Error: 'shared' cannot be placed after a template constraint
fail_compilation/test4946.d(18): Error: 'nothrow' cannot be placed after a template constraint
fail_compilation/test4946.d(19): Error: attributes cannot be placed after a template constraint
---
*/

void bar1(int x)() if (x > 0) pure { int a;}
void bar2(int x)() if (x > 0) const { int a;}
void bar3(int x)() if (x > 0) immutable { int a;}
void bar4(int x)() if (x > 0) inout { int a;}
void bar5(int x)() if (x > 0) shared { int a;}
void bar6(int x)() if (x > 0) nothrow { int a;}
void bar7(int x)() if (x > 0) @safe { int a;}
