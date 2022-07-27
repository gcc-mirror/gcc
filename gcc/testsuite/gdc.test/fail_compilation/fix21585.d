/* https://issues.dlang.org/show_bug.cgi?id=21585
TEST_OUTPUT:
---
fail_compilation/fix21585.d(103): Error: expected 1 arguments for `toType` but had 0
fail_compilation/fix21585.d(104): Error: expression expected as second argument of __traits `toType`
fail_compilation/fix21585.d(105): Error: `string` expected for __traits(toType, string), not `(1)` of type `int`
fail_compilation/fix21585.d(106): Error: cannot determine `__traits(toType, "hello betty")`
---
*/

#line 100

template Type(T) { alias Type = T; }

alias T1 = Type!(__traits(toType));
alias T2 = Type!(__traits(toType, int));
alias T3 = Type!(__traits(toType, 1));
alias T4 = Type!(__traits(toType, "hello betty"));
