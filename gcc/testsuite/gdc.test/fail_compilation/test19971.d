/* TEST_OUTPUT:
---
fail_compilation/test19971.d(16): Error: function `f` is not callable using argument types `(string)`
fail_compilation/test19971.d(16):        cannot pass argument `"%s"` of type `string` to parameter `int x`
fail_compilation/test19971.d(13):        `test19971.f(int x)` declared here
fail_compilation/test19971.d(17): Error: function literal `__lambda_L17_C5(int x)` is not callable using argument types `(string)`
fail_compilation/test19971.d(17):        cannot pass argument `"%s"` of type `string` to parameter `int x`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=19971

void f(int x) {}
void main()
{
    f("%s");
    (int x) {} ("%s");
}
