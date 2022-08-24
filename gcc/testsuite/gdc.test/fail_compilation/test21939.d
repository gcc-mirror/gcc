// https://issues.dlang.org/show_bug.cgi?id=21939
/*
TEST_OUTPUT:
---
fail_compilation/test21939.d(11): Error: invalid `foreach` aggregate `Object` of type `Object`
fail_compilation/test21939.d(11):        `foreach` works with input ranges (implementing `front` and `popFront`), aggregates implementing `opApply`, or the result of an aggregate's `.tupleof` property
fail_compilation/test21939.d(11):        https://dlang.org/phobos/std_range_primitives.html#isInputRange
---
*/

static foreach (a; Object) {}
