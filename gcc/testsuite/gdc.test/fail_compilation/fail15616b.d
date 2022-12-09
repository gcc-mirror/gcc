/*
REQUIRED_ARGS: -v
TRANSFORM_OUTPUT: remove_lines("^(predefs|binary|version|config|DFLAG|parse|import|semantic|entry|\s*$)")
TEST_OUTPUT:
---
fail_compilation/fail15616b.d(44): Error: none of the overloads of `foo` are callable using argument types `(double)`
fail_compilation/fail15616b.d(17):        Candidates are: `fail15616b.foo(int a)`
fail_compilation/fail15616b.d(20):                        `fail15616b.foo(int a, int b)`
fail_compilation/fail15616b.d(29):                        `fail15616b.foo(int a, int b, int c)`
fail_compilation/fail15616b.d(32):                        `fail15616b.foo(string a)`
fail_compilation/fail15616b.d(35):                        `fail15616b.foo(string a, string b)`
fail_compilation/fail15616b.d(38):                        `fail15616b.foo(string a, string b, string c)`
fail_compilation/fail15616b.d(23):                        `foo(T)(T a)`
  with `T = double`
  whose parameters have the following constraints:
  `~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
`  > is(T == float)
`  `~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
fail_compilation/fail15616b.d(26):                        `foo(T)(T a)`
  with `T = double`
  whose parameters have the following constraints:
  `~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
`  > is(T == char)
`  `~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
  Tip: not satisfied constraints are marked with `>`
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
