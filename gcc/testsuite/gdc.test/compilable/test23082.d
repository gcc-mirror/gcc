// https://issues.dlang.org/show_bug.cgi?id=23082

/*
TEST_OUTPUT:
---
bar
---
*/

void foo()() {}
alias bar = foo;
void bar() { }

void main()
{
    pragma(msg, __traits(parent, main).bar.stringof);
}
