// REQUIRED_ARGS: -O
// https://issues.dlang.org/show_bug.cgi?id=20990

// foo() and bar() should produce the same code when
// optimized.

void foo(int* ptr)
{
    if (ptr is null)
        assert(false);
    *ptr = 42;
}

void bar(int* ptr)
{
    assert(ptr);
    *ptr = 42;
}
