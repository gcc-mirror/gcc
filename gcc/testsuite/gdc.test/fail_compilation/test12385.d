/*
TEST_OUTPUT:
---
fail_compilation/test12385.d(29): Error: cannot modify `immutable` expression `unbundled.x`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=12385

class BundledState
{
    string m_State;

    int x = 3;

    this(string state) immutable
    {
        m_State = state;
    }
}

enum States : immutable(BundledState)
{
    unbundled = new immutable BundledState("bla"),
}

void main()
{
    States.unbundled.x = 6; // Modifies x.
}
