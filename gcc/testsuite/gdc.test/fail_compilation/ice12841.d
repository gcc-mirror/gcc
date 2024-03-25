/*
TEST_OUTPUT:
---
fail_compilation/ice12841.d(23): Error: cannot take address of expression `taskPool().amap(Args...)(Args args)` because it is not an lvalue
fail_compilation/ice12841.d(24): Error: cannot take address of template `amap(Args...)(Args args)`, perhaps instantiate it first
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
