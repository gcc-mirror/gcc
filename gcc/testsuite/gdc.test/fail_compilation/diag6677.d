/*
TEST_OUTPUT:
---
fail_compilation/diag6677.d(17): Error: static constructor cannot be `const`
fail_compilation/diag6677.d(18): Error: static constructor cannot be `inout`
fail_compilation/diag6677.d(19): Error: static constructor cannot be `immutable`
fail_compilation/diag6677.d(20): Error: use `shared static this()` to declare a shared static constructor
fail_compilation/diag6677.d(21): Error: use `shared static this()` to declare a shared static constructor
fail_compilation/diag6677.d(23): Error: shared static constructor cannot be `const`
fail_compilation/diag6677.d(24): Error: shared static constructor cannot be `inout`
fail_compilation/diag6677.d(25): Error: shared static constructor cannot be `immutable`
fail_compilation/diag6677.d(26): Error: redundant attribute `shared`
fail_compilation/diag6677.d(27): Error: redundant attribute `shared`
---
*/

static this() const { }
static this() inout { }
static this() immutable { }
static this() shared { }
static this() const shared { }

shared static this() const { }
shared static this() inout { }
shared static this() immutable { }
shared static this() shared { }
shared static this() const shared { }
