/**
TEST_OUTPUT:
---
fail_compilation/standalone_modctor.d(11): Error: `@standalone` can only be used on shared static constructors
fail_compilation/standalone_modctor.d(12): Error: a module constructor using `@standalone` must be `@system` or `@trusted`
fail_compilation/standalone_modctor.d(13): Error: a module constructor using `@standalone` must be `@system` or `@trusted`
---
*/
import core.attribute : standalone;

@standalone        static this() {}
@standalone shared static this() {}
@standalone shared static this() @safe {}
@standalone shared static this() @trusted {}
@standalone shared static this() @system {}
