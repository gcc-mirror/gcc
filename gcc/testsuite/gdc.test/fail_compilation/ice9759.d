/*
TEST_OUTPUT:
---
fail_compilation/ice9759.d(25): Error: mutable method `ice9759.Json.opAssign` is not callable using a `const` object
fail_compilation/ice9759.d(17):        Consider adding `const` or `inout` here
---
*/

struct Json
{
    union
    {
        Json[] m_array;
        Json[string] m_object;
    }

    void opAssign(Json v)
    {
    }
}

void bug()
{
    const(Json) r;
    r = r.init;
}
