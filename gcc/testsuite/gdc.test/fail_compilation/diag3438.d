// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/diag3438.d(16): Deprecation: constructor diag3438.F1.this all parameters have default arguments, but structs cannot have default constructors.
fail_compilation/diag3438.d(17): Deprecation: constructor diag3438.F2.this all parameters have default arguments, but structs cannot have default constructors.
fail_compilation/diag3438.d(20): Deprecation: constructor diag3438.F5.this @disable'd constructor cannot have default arguments for all parameters.
fail_compilation/diag3438.d(20):        Use @disable this(); if you want to disable default initialization.
fail_compilation/diag3438.d(21): Deprecation: constructor diag3438.F6.this @disable'd constructor cannot have default arguments for all parameters.
fail_compilation/diag3438.d(21):        Use @disable this(); if you want to disable default initialization.
---
*/

import core.vararg;

struct F1 { this(int x = 1) { } }
struct F2 { this(int x = 1, ...) { } }
struct F3 { this(...) { } } // ok
struct F4 { this(int[] x...) { } }  // ok
struct F5 { @disable this(int x = 1); }
struct F6 { @disable this(int x = 1) { } }
