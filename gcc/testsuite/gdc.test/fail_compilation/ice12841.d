/*
TEST_OUTPUT:
---
fail_compilation/ice12841.d(23): Error: `taskPool().amap(Args...)(Args args)` is not an lvalue and cannot be modified
fail_compilation/ice12841.d(24): Error: `amap(Args...)(Args args)` is not an lvalue and cannot be modified
---
*/

@property TaskPool taskPool() @trusted { return new TaskPool; }

final class TaskPool
{
    template amap(functions...)
    {
        auto amap(Args...)(Args args)
        {
        }
    }
}

void main()
{
    auto dg = &(taskPool.amap!"a.result()");
    auto fp = &(TaskPool.amap!"a.result()");
}
