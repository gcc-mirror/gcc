/*
TEST_OUTPUT:
---
fail_compilation/fail18985.d(20): Error: operator `+=` not supported for `foo` of type `C`
fail_compilation/fail18985.d(13):        perhaps implement `auto opOpAssign(string op : "+")(int) {}`
fail_compilation/fail18985.d(21): Error: operator `+=` not supported for `bar` of type `C`
fail_compilation/fail18985.d(13):        perhaps implement `auto opOpAssign(string op : "+")(int) {}`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=18985

class C {}

C foo;
shared C bar;

void main()
{
    foo += 1;
    bar += 1;
}
