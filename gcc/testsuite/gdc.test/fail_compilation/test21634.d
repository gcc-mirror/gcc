/*
REQUIRED_ARGS: -verrors=context
TEST_OUTPUT:
---
fail_compilation/test21634.d(22): Error: function literal `(int x) { }` is not callable using argument types `(string)`
    (int x) {} ("%s");
               ^
fail_compilation/test21634.d(22):        cannot pass argument `"%s"` of type `string` to parameter `int x`
    (int x) {} ("%s");
               ^
fail_compilation/test21634.d(24): Error: declaration `test21634.main.foo` is already defined
    int foo;
    ^
fail_compilation/test21634.d(23):        `variable` `foo` is defined here
    float foo;
          ^
---
*/
void f(int x) {}
void main()
{
    (int x) {} ("%s");
    float foo;
    int foo;
}
