/*
REQUIRED_ARGS: -verror-supplements=2
TEST_OUTPUT:
---
fail_compilation/fail15616d.d(44): Error: none of the overloads of `foo` are callable using argument types `(double)`
fail_compilation/fail15616d.d(17):        Candidates are: `fail15616d.foo(int a)`
fail_compilation/fail15616d.d(20):                        `fail15616d.foo(int a, int b)`
fail_compilation/fail15616d.d(44):        ... (6 more, -v to show) ...
---
*/

#line 17
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
