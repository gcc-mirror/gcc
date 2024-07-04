/**
TEST_OUTPUT:
---
Call 0
(x: true, y: false)
Call 1
Call 2
(x: false, y: false)
Call 3
(x: false, y: true)
Call 4
---

As of writing this test, template function instances store the function arguments from the call site.
When looking in cache for an existing template instantiation, matching template arguments isn't
sufficient: `auto ref` parameters being ref or not also create different template instances.
This test checks that the special logic for it still works with named arguments.
*/

void autoref()(auto ref int x, auto ref int y)
{
    pragma(msg, "(x: ", __traits(isRef, x), ", y: ", __traits(isRef, y), ")");
}

void main()
{
    int REF = 0;
    enum NOT = 0;
    pragma(msg, "Call 0");
    autoref(y: NOT, x: REF); // new instance
    pragma(msg, "Call 1");
    autoref(x: REF, y: NOT); // existing instance
    pragma(msg, "Call 2");
    autoref(x: NOT, y: NOT); // new instance
    pragma(msg, "Call 3");
    autoref(x: NOT, y: REF); // new instance
    pragma(msg, "Call 4");
    autoref(y: REF, x: NOT); // existing instance
}
