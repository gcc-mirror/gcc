/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test22910.d(17): Error: escaping a reference to parameter `this` by returning `&this.val` is not allowed in a `@safe` function
fail_compilation/test22910.d(15):        perhaps change the `return scope` into `scope return`
---
*/
@safe:

struct S
{
    int  val;
    int* ptr;

    int* retScope() return scope
    {
        return &this.val;
    }
}
