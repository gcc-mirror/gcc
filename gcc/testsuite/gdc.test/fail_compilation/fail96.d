/*
TEST_OUTPUT:
---
fail_compilation/fail96.d(21): Error: template instance `foo!long` `foo` is not a template declaration, it is a function alias
---
*/

// https://issues.dlang.org/show_bug.cgi?id=153

template bar(T)
{
    void foo() {}
}

alias bar!(long).foo foo;
alias bar!(char).foo foo;


void main()
{
    foo!(long);
}
