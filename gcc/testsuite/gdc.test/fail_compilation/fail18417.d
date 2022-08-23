// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/fail18417.d(11): Deprecation: `const` postblit is deprecated. Please use an unqualified postblit.
fail_compilation/fail18417.d(12): Deprecation: `immutable` postblit is deprecated. Please use an unqualified postblit.
fail_compilation/fail18417.d(13): Deprecation: `shared` postblit is deprecated. Please use an unqualified postblit.
---
*/

struct A { this(this) const {} }
struct B { this(this) immutable {} }
struct C { this(this) shared {} }
