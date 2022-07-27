/*
TEST_OUTPUT:
---
fail_compilation/fail72.d(10): Error: undefined identifier `foo`
---
*/

void main()
{
    synchronized( foo )
    {

    }
}
