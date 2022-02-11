/*
TEST_OUTPUT:
---
fail_compilation/fail2456.d(14): Error: cannot put `scope(success)` statement inside `finally` block
---
*/
void test_success()
{
    try
    {
    }
    finally
    {
        scope(success) {}           // NG
    }
}

/*
TEST_OUTPUT:
---
fail_compilation/fail2456.d(31): Error: cannot put `scope(failure)` statement inside `finally` block
---
*/
void test_failure()
{
    try
    {
    }
    finally
    {
        scope(failure) {}           // NG
    }
}

/*
TEST_OUTPUT:
---
---
*/
void test_exit()
{
    try
    {
    }
    finally
    {
        scope(exit) {}              // OK
    }
}

/*
TEST_OUTPUT:
---
fail_compilation/fail2456.d(64): Error: cannot put `scope(success)` statement inside `scope(success)`
fail_compilation/fail2456.d(65): Error: cannot put `scope(failure)` statement inside `scope(success)`
fail_compilation/fail2456.d(78): Error: cannot put `scope(success)` statement inside `scope(exit)`
fail_compilation/fail2456.d(79): Error: cannot put `scope(failure)` statement inside `scope(exit)`
---
*/
void test2456a()
{
    scope(success)
    {
        scope(success) {}   // NG
        scope(failure) {}   // NG
        scope(exit) {}      // OK
    }

    scope(failure)
    {
        scope(success) {}   // OK
        scope(failure) {}   // OK
        scope(exit) {}      // OK
    }

    scope(exit)
    {
        scope(success) {}   // NG
        scope(failure) {}   // NG
        scope(exit) {}      // OK
    }
}

/*
TEST_OUTPUT:
---
fail_compilation/fail2456.d(96): Error: cannot put `catch` statement inside `scope(success)`
fail_compilation/fail2456.d(108): Error: cannot put `catch` statement inside `scope(exit)`
---
*/
void test2456b()
{
    scope(success)
    {
        try {}
        catch (Throwable) {}    // NG
    }

    scope(failure)
    {
        try {}
        catch (Throwable) {}    // OK
    }

    scope(exit)
    {
        try {}
        catch (Throwable) {}    // NG
    }
}
