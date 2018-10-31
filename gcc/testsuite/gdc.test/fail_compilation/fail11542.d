// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

/*
TEST_OUTPUT:
---
fail_compilation/fail11542.d(16): Error: `object.Exception` is thrown but not caught
fail_compilation/fail11542.d(13): Error: nothrow function `fail11542.test_success1` may throw
fail_compilation/fail11542.d(26): Error: `object.Exception` is thrown but not caught
fail_compilation/fail11542.d(23): Error: nothrow function `fail11542.test_success3` may throw
---
*/
void test_success1() nothrow
{
    scope(success) {}
    throw new Exception("");    // error
}
void test_success2() nothrow
{
    scope(success) {}
    throw new Error("");        // no error
}
void test_success3() nothrow
{
    scope(success) assert(0);
    throw new Exception("");    // error
}

/*
TEST_OUTPUT:
---
fail_compilation/fail11542.d(39): Error: `object.Exception` is thrown but not caught
fail_compilation/fail11542.d(36): Error: nothrow function `fail11542.test_failure1` may throw
---
*/
void test_failure1() nothrow
{
    scope(failure) {}
    throw new Exception("");    // error
}
void test_failure2() nothrow
{
    scope(failure) {}
    throw new Error("");        // no error
}
void est_failure3() nothrow
{
    scope(failure) assert(0);
    throw new Exception("");    // no error
}

/*
TEST_OUTPUT:
---
fail_compilation/fail11542.d(62): Error: `object.Exception` is thrown but not caught
fail_compilation/fail11542.d(59): Error: nothrow function `fail11542.test_exit1` may throw
---
*/
void test_exit1() nothrow
{
    scope(exit) {}
    throw new Exception("");    // error
}
void test_exit2() nothrow
{
    scope(exit) {}
    throw new Error("");        // no error
}
void test_exit3() nothrow
{
    scope(exit) assert(0);
    throw new Exception("");    // no error
}
