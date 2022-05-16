/* https://issues.dlang.org/show_bug.cgi?id=23097
TEST_OUTPUT:
---
fail_compilation/ice23097.d(12): Error: undefined identifier `ICE`
fail_compilation/ice23097.d(27): Error: template instance `ice23097.ice23097!(S23097)` error instantiating
fail_compilation/ice23097.d(27): Error: function `ice23097.ice23097!(S23097).ice23097(S23097 _param_0)` is not callable using argument types `(S23097)`
fail_compilation/ice23097.d(27):        generating a copy constructor for `struct S23097` failed, therefore instances of it are uncopyable
---
*/
auto ice23097(I)(I)
{
    ICE;
}

struct Cpctor23097
{
    this(ref typeof(this)) { }
}

struct S23097
{
    Cpctor23097 cpctor;
}

auto fail23097(S23097 s)
{
    s.ice23097;
}
