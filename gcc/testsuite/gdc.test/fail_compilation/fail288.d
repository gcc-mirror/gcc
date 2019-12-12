/*
TEST_OUTPUT:
---
fail_compilation/fail288.d(14): Error: case ranges not allowed in final switch
---
*/

void main()
{
    enum E { a, b }
    E i = E.a;
    final switch (i)
    {
        case E.a: .. case E.b:
            break;
    }
}
