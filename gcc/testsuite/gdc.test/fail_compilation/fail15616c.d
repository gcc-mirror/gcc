/*
REQUIRED_ARGS: -verror-supplements=0
TEST_OUTPUT:
---
fail_compilation/fail15616c.d(44): Error: none of the overloads of `foo` are callable using argument types `(double)`
fail_compilation/fail15616c.d(17):        Candidates are: `fail15616c.foo(int a)`
fail_compilation/fail15616c.d(20):                        `fail15616c.foo(int a, int b)`
fail_compilation/fail15616c.d(29):                        `fail15616c.foo(int a, int b, int c)`
fail_compilation/fail15616c.d(32):                        `fail15616c.foo(string a)`
fail_compilation/fail15616c.d(35):                        `fail15616c.foo(string a, string b)`
fail_compilation/fail15616c.d(38):                        `fail15616c.foo(string a, string b, string c)`
fail_compilation/fail15616c.d(23):                        `foo(T)(T a)`
  with `T = double`
  must satisfy the following constraint:
`       is(T == float)`
fail_compilation/fail15616c.d(26):                        `foo(T)(T a)`
  with `T = double`
  must satisfy the following constraint:
`       is(T == char)`
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
