/*
TEST_OUTPUT:
---
fail_compilation/test12558.d(32): Deprecation: `catch` statement without an exception specification is deprecated
fail_compilation/test12558.d(32):        use `catch(Throwable)` for old behavior
fail_compilation/test12558.d(36): Deprecation: `catch` statement without an exception specification is deprecated
fail_compilation/test12558.d(36):        use `catch(Throwable)` for old behavior
fail_compilation/test12558.d(43): Deprecation: `catch` statement without an exception specification is deprecated
fail_compilation/test12558.d(43):        use `catch(Throwable)` for old behavior
fail_compilation/test12558.d(47): Deprecation: `catch` statement without an exception specification is deprecated
fail_compilation/test12558.d(47):        use `catch(Throwable)` for old behavior
fail_compilation/test12558.d(56): Deprecation: `catch` statement without an exception specification is deprecated
fail_compilation/test12558.d(56):        use `catch(Throwable)` for old behavior
fail_compilation/test12558.d(31): Error: `catch` statement without an exception specification is deprecated
fail_compilation/test12558.d(31):        use `catch(Throwable)` for old behavior
fail_compilation/test12558.d(36): Error: `catch` statement without an exception specification is deprecated
fail_compilation/test12558.d(36):        use `catch(Throwable)` for old behavior
fail_compilation/test12558.d(42): Error: `catch` statement without an exception specification is deprecated
fail_compilation/test12558.d(42):        use `catch(Throwable)` for old behavior
fail_compilation/test12558.d(47): Error: `catch` statement without an exception specification is deprecated
fail_compilation/test12558.d(47):        use `catch(Throwable)` for old behavior
---
*/

void main()
{
    auto handler = () { };

    try {
        assert(0);
    } catch
        handler();

    try {
        assert(0);
    } catch {
        handler();
    }

    try {
        assert(0);
    } catch
        handler();

    try {
        assert(0);
    } catch {
        handler();
    }
}

void foo()()
{
    try {}
    catch
        assert(false);
}
