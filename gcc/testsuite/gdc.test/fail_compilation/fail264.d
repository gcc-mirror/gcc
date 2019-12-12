/*
TEST_OUTPUT:
---
fail_compilation/fail264.d(10): Error: undefined identifier `undef`
---
*/

void main()
{
    foreach (element; undef)
    {
        fn(element);
    }
}

void fn(int i) {}
