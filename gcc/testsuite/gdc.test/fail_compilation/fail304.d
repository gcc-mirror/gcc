/*
TEST_OUTPUT:
---
fail_compilation/fail304.d(15): Error: cannot cast expression `foo()` of type `Small` to `Large` because of different sizes
---
*/

struct Small { uint x; }
struct Large { uint x, y, z; }
Small foo() { return Small(); }
void main()
{
    Large l;
    Small s;
    l = cast(Large)foo();
}
