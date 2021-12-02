/*
TEST_OUTPUT:
---
fail_compilation/fail14965.d(19): Error: forward reference to inferred return type of function `foo1`
fail_compilation/fail14965.d(20): Error: forward reference to inferred return type of function `foo2`
fail_compilation/fail14965.d(22): Error: forward reference to inferred return type of function `bar1`
fail_compilation/fail14965.d(23): Error: forward reference to inferred return type of function `bar2`
fail_compilation/fail14965.d(25): Error: forward reference to inferred return type of function `baz1`
fail_compilation/fail14965.d(26): Error: forward reference to inferred return type of function `baz2`
fail_compilation/fail14965.d(30): Error: forward reference to inferred return type of function `foo1`
fail_compilation/fail14965.d(31): Error: forward reference to inferred return type of function `foo2`
fail_compilation/fail14965.d(33): Error: forward reference to inferred return type of function `bar1`
fail_compilation/fail14965.d(34): Error: forward reference to inferred return type of function `bar2`
fail_compilation/fail14965.d(36): Error: forward reference to inferred return type of function `baz1`
fail_compilation/fail14965.d(37): Error: forward reference to inferred return type of function `baz2`
---
*/

auto foo1() { alias F = typeof(foo1); }     // TypeTypeof
auto foo2() { alias FP = typeof(&foo2); }   // TypeTypeof

auto bar1() { auto fp = &bar1; }            // ExpInitializer
auto bar2() { auto fp = cast(void function())&bar2; }   // castTo

auto baz1() { return &baz1; }               // ReturnStatement
auto baz2() { (&baz2); }                    // ExpStatement

class C
{
    auto foo1() { alias F = typeof(this.foo1); }
    auto foo2() { alias FP = typeof(&this.foo2); }

    auto bar1() { auto fp = &this.bar1; }
    auto bar2() { auto dg = cast(void delegate())&this.bar2; }

    auto baz1() { return &baz1; }
    auto baz2() { (&baz2); }
}
