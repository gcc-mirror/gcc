// https://issues.dlang.org/show_bug.cgi?id=22134
/* REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/fail22134.d(12): Deprecation: `this.arr[i]` has no effect
---
*/
struct StackBuffer
{
    auto opIndex(size_t i)
    {
        return arr[i];
    }

private:
    void[] arr;
}
