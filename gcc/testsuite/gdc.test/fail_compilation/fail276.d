/*
TEST_OUTPUT:
---
fail_compilation/fail276.d(19): Error: `this` has no effect
fail_compilation/fail276.d(15): Error: cannot construct anonymous nested class because no implicit `this` reference to outer class is available
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
