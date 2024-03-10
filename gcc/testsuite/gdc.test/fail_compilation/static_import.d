/*
TEST_OUTPUT:
---
fail_compilation/static_import.d(8): Error: static import `core` cannot have an import bind list
---
*/

static import core.stdc.stdio : p = q;
