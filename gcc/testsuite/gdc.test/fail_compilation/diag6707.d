/*
TEST_OUTPUT:
---
fail_compilation/diag6707.d(17): Error: mutable method `diag6707.Foo.value` is not callable using a `const` object
fail_compilation/diag6707.d(13):        Consider adding `const` or `inout` here
---
*/

module diag6707;

struct Foo
{
    @property bool value() { return true; }

    void test() const
    {
        auto x = value;
    }
}
