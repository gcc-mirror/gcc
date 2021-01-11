/*
TEST_OUTPUT:
---
fail_compilation/fail276.d(19): Error: `this` has no effect
fail_compilation/fail276.d(15): Error: `this` is only defined in non-static member functions, not `__anonclass2`
---
*/

class C
{
    this()
    {
        auto i = new class()
        {
            auto k = new class()
            {
                void func()
                {
                    this.outer.outer;
                }
            };
        };
    }
    int i;
}
void main() {}
