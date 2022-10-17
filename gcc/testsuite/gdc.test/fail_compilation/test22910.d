/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test22910.d(17): Error: returning `&this.val` escapes a reference to parameter `this`
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
