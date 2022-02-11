/*
TEST_OUTPUT:
---
fail_compilation/diag10688.d(12): Error: function `diag10688.Bar.foo` `private` method is not virtual and cannot override
fail_compilation/diag10688.d(14): Error: function `diag10688.Bar.bar` `package` method is not virtual and cannot override
---
*/

class Bar
{
private:
    override void foo() { }
package:
    override void bar() { }
}
