/*
REQUIRED_ARGS: -v
---
fail_compilation/fail15616b.d(43): Error: none of the overloads of 'foo' are callable using argument types (double), candidates are:
fail_compilation/fail15616b.d(16):        fail15616b.foo(int a)
fail_compilation/fail15616b.d(19):        fail15616b.foo(int a, int b)
fail_compilation/fail15616b.d(28):        fail15616b.foo(int a, int b, int c)
fail_compilation/fail15616b.d(31):        fail15616b.foo(string a)
fail_compilation/fail15616b.d(34):        fail15616b.foo(string a, string b)
fail_compilation/fail15616b.d(37):        fail15616b.foo(string a, string b, string c)
fail_compilation/fail15616b.d(22):        fail15616b.foo(T)(T a) if (is(T == float))
fail_compilation/fail15616b.d(25):        fail15616b.foo(T)(T a) if (is(T == char))
---
*/

void foo(int a)
{}

void foo(int a, int b)
{}

void foo(T)(T a) if (is(T == float))
{}

void foo(T)(T a) if (is(T == char))
{}

void foo(int a, int b, int c)
{}

void foo(string a)
{}

void foo(string a, string b)
{}

void foo(string a, string b, string c)
{}


void main()
{
    foo(3.14);
}
