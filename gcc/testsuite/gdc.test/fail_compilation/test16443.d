/* TEST_OUTPUT:
---
fail_compilation/test16443.d(10): Error: incompatible types for `(null) + (null)`: both operands are of type `typeof(null)`
fail_compilation/test16443.d(11): Error: incompatible types for `(null) - (null)`: both operands are of type `typeof(null)`
---
*/

void foo()
{
    auto a = null + null;
    auto b = null - null;
}
