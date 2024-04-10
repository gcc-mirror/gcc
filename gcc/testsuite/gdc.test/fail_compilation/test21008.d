/*
TEST_OUTPUT:
---
fail_compilation/test21008.d(110): Error: function `test21008.C.after` circular reference to class `C`
fail_compilation/test21008.d(117): Error: calling non-static function `toString` requires an instance of type `Object`
fail_compilation/test21008.d(117): Error: calling non-static function `toHash` requires an instance of type `Object`
fail_compilation/test21008.d(117): Error: function `opCmp` is not callable using argument types `()`
fail_compilation/test21008.d(117):        too few arguments, expected 1, got 0
$p:druntime/import/object.d$($n$):        `object.Object.opCmp(Object o)` declared here
fail_compilation/test21008.d(117): Error: function `opEquals` is not callable using argument types `()`
fail_compilation/test21008.d(117):        too few arguments, expected 1, got 0
$p:druntime/import/object.d$($n$):        `object.Object.opEquals(Object o)` declared here
fail_compilation/test21008.d(117): Error: `Monitor` has no effect
fail_compilation/test21008.d(117): Error: function `factory` is not callable using argument types `()`
fail_compilation/test21008.d(117):        too few arguments, expected 1, got 0
$p:druntime/import/object.d$($n$):        `object.Object.factory(string classname)` declared here
fail_compilation/test21008.d(105):        called from here: `handleMiddlewareAnnotation()`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=21008

#line 100

class Base
{
    bool after();

    mixin(handleMiddlewareAnnotation);
}

class C : Base
{
    override bool after();
}

string handleMiddlewareAnnotation()
{
    foreach (member; __traits(allMembers, C))
    {
        __traits(getMember, C, member);
    }
}
