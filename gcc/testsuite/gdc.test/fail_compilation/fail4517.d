/*
TEST_OUTPUT:
---
fail_compilation/fail4517.d(16): Error: `enum` member `B` not represented in `final switch`
---
*/

enum E : ushort
{
    A, B
}

void main()
{
    E e;
    final switch(e)
    {
        case E.A:
            break;
    }
}
