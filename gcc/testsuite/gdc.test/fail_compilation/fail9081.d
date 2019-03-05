/*
TEST_OUTPUT:
---
fail_compilation/fail9081.d(12): Error: package core has no type
fail_compilation/fail9081.d(13): Error: package stdc has no type
fail_compilation/fail9081.d(14): Error: module stdio has no type
---
*/

import core.stdc.stdio;

typeof(core) a;
typeof(core.stdc) b;
typeof(core.stdc.stdio) c;
