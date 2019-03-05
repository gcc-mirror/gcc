// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

bool cond;

/*
TEST_OUTPUT:
---
fail_compilation/fail12809.d(19): Error: `object.Exception` is thrown but not caught
fail_compilation/fail12809.d(16): Error: nothrow function `fail12809.test_finally1` may throw
fail_compilation/fail12809.d(35): Error: `object.Exception` is thrown but not caught
fail_compilation/fail12809.d(39): Error: `object.Exception` is thrown but not caught
fail_compilation/fail12809.d(32): Error: nothrow function `fail12809.test_finally3` may throw
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
fail_compilation/fail12809.d(59): Error: `object.Exception` is thrown but not caught
fail_compilation/fail12809.d(54): Error: nothrow function `fail12809.test_finally4` may throw
fail_compilation/fail12809.d(75): Error: `object.Exception` is thrown but not caught
fail_compilation/fail12809.d(79): Error: `object.Exception` is thrown but not caught
fail_compilation/fail12809.d(70): Error: nothrow function `fail12809.test_finally6` may throw
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
