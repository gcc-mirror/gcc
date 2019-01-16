// REQUIRED_ARGS: -dw
// PERMUTE_ARGS:
/*
TEST_OUTPUT:
---
---
*/

class C
{
    void bug()
    {
        autoref(this);  // 'auto ref' becomes non-ref parameter
        autoref(super); // 'auto ref' becomes non-ref parameter
    }
}

void autoref(T)(auto ref T t) { static assert(__traits(isRef, t) == false); }
