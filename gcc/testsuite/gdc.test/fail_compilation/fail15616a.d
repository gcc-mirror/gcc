/*
TEST_OUTPUT:
---
fail_compilation/fail15616a.d(41): Error: none of the overloads of `foo` are callable using argument types `(double)`
fail_compilation/fail15616a.d(14):        Candidates are: `fail15616a.foo(int a)`
fail_compilation/fail15616a.d(17):                        `fail15616a.foo(int a, int b)`
fail_compilation/fail15616a.d(26):                        `fail15616a.foo(int a, int b, int c)`
fail_compilation/fail15616a.d(29):                        `fail15616a.foo(string a)`
fail_compilation/fail15616a.d(32):                        `fail15616a.foo(string a, string b)`
fail_compilation\fail15616a.d(35):                        `fail15616a.foo(string a, string b, string c)`
fail_compilation/fail15616a.d(41):        ... (2 more, -v to show) ...
---
*/
#line 14
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
