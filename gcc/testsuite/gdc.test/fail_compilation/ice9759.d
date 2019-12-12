/*
TEST_OUTPUT:
---
fail_compilation/ice9759.d(24): Error: mutable method ice9759.Json.opAssign is not callable using a const object
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
