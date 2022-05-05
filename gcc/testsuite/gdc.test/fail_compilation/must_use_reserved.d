/+
TEST_OUTPUT:
---
fail_compilation/must_use_reserved.d(14): Error: `@mustuse` on `class` types is reserved for future use
fail_compilation/must_use_reserved.d(15): Error: `@mustuse` on `interface` types is reserved for future use
fail_compilation/must_use_reserved.d(16): Error: `@mustuse` on `enum` types is reserved for future use
fail_compilation/must_use_reserved.d(17): Error: `@mustuse` on functions is reserved for future use
fail_compilation/must_use_reserved.d(19): Error: `@mustuse` on `class` types is reserved for future use
fail_compilation/must_use_reserved.d(20): Error: template instance `must_use_reserved.CT!int` error instantiating
---
+/
import core.attribute;

@mustuse class C {}
@mustuse interface I {}
@mustuse enum E { x }
@mustuse int fun() { return 0; }

@mustuse class CT(T) {}
alias _ = CT!int;
