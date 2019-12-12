/* TEST_OUTPUT:
---
fail_compilation/fail17421.d(14): Error: argument to `__traits(getFunctionVariadicStyle, 1)` is not a function
fail_compilation/fail17421.d(14):        while evaluating: `static assert(__traits(getFunctionVariadicStyle, 1) == "none")`
fail_compilation/fail17421.d(15): Error: argument to `__traits(getFunctionVariadicStyle, int*)` is not a function
fail_compilation/fail17421.d(15):        while evaluating: `static assert(__traits(getFunctionVariadicStyle, int*) == "none")`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=17421

alias int* x;

static assert(__traits(getFunctionVariadicStyle, 1) == "none");
static assert(__traits(getFunctionVariadicStyle, x) == "none");

