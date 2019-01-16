/*
TEST_OUTPUT:
---
fail_compilation/fail92.d(15): Error: invalid foreach aggregate `t`
fail_compilation/fail92.d(23): Error: template instance fail92.crash!(typeof(null)) error instantiating
---
*/

// [25]

template crash(T)
{
    void crash(T t)
    {
        foreach (u; t)
        {
        }
    }
}

void main()
{
    crash(null);
}
