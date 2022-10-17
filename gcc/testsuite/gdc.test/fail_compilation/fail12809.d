// REQUIRED_ARGS: -o-

bool cond;

/*
TEST_OUTPUT:
---
fail_compilation/fail12809.d(18): Error: `object.Exception` is thrown but not caught
fail_compilation/fail12809.d(15): Error: function `fail12809.test_finally1` may throw but is marked as `nothrow`
fail_compilation/fail12809.d(34): Error: `object.Exception` is thrown but not caught
fail_compilation/fail12809.d(38): Error: `object.Exception` is thrown but not caught
fail_compilation/fail12809.d(31): Error: function `fail12809.test_finally3` may throw but is marked as `nothrow`
---
*/
void test_finally1() nothrow
{
    try
        throw new Exception("");        // error
    finally
    {}
}

void test_finally2() nothrow
{
    try
        throw new Exception("");        // no error
    finally
        assert(0);  // unconditional halt
}

void test_finally3() nothrow
{
    try
        throw new Exception("");        // error
    finally
    {
        if (cond)
            throw new Exception("");    // error
        assert(0);  // conditional halt
    }
}

/*
TEST_OUTPUT:
---
fail_compilation/fail12809.d(58): Error: `object.Exception` is thrown but not caught
fail_compilation/fail12809.d(53): Error: function `fail12809.test_finally4` may throw but is marked as `nothrow`
fail_compilation/fail12809.d(74): Error: `object.Exception` is thrown but not caught
fail_compilation/fail12809.d(78): Error: `object.Exception` is thrown but not caught
fail_compilation/fail12809.d(69): Error: function `fail12809.test_finally6` may throw but is marked as `nothrow`
---
*/
void test_finally4() nothrow
{
    try
    {}
    finally
        throw new Exception("");        // error
}

void test_finally5() nothrow
{
    try
        assert(0);  // unconditional halt
    finally
        throw new Exception("");        // no error
}

void test_finally6() nothrow
{
    try
    {
        if (cond)
            throw new Exception("");    // error
        assert(0);  // conditional halt
    }
    finally
        throw new Exception("");        // error
}
