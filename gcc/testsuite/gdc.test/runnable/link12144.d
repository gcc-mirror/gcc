// COMPILE_SEPARATELY: -g
// EXTRA_SOURCES: imports/link12144a.d
/*
TEST_OUTPUT:
---
runnable/imports/link12144a.d(31): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
---
*/

import imports.link12144a;

void main()
{
    fun();
}

struct A12146
{
    B12146[] tokens;
    // implicitly generated
    //   bool opEquals(const ref Appender rhs) const
    // will make
    //   tokens == rhs.tokens
    // references TypeInfo of B12146
    // and it references __xopCmp
}
