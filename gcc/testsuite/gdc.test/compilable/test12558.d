// REQUIRED_ARGS:
/*
TEST_OUTPUT:
---
compilable/test12558.d(16): Deprecation: catch statement without an exception specification is deprecated; use catch(Throwable) for old behavior
compilable/test12558.d(21): Deprecation: catch statement without an exception specification is deprecated; use catch(Throwable) for old behavior
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

    // ensure diagnostics are not emitted for verioned-out blocks
    version (none)
    {
        try {
            assert(0);
        } catch  // should not emit diagnostics
            handler();

        try {
            assert(0);
        } catch {  // ditto
            handler();
        }
    }
}
