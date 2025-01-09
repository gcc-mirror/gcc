/*
TEST_OUTPUT:
---
fail_compilation/fail8313.d(13): Error: `fail8313.bar` called with argument types `(int)` matches multiple overloads exactly:
fail_compilation/fail8313.d(11):     `fail8313.bar!().bar(int x)`
and:
fail_compilation/fail8313.d(12):     `fail8313.bar!().bar(int x)`
fail_compilation/fail8313.d(13):        while evaluating: `static assert(bar()(int x)(1))`
---
*/
auto bar()(int x){return x;}
auto bar()(int x = bar()){return x;}
static assert(bar(1));
