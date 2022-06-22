/*
REQUIRED_ARGS: -preview=in -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/previewin2.d(1): Error: cannot use `in` parameters with `extern(C)` functions
fail_compilation/previewin2.d(1):        parameter `a` declared as `in` here
fail_compilation/previewin2.d(2): Error: cannot use `in` parameters with `extern(Windows)` functions
fail_compilation/previewin2.d(2):        parameter `a` declared as `in` here
fail_compilation/previewin2.d(4): Error: cannot use `in` parameters with `extern(C)` functions
fail_compilation/previewin2.d(4):        parameter `__anonymous_param` declared as `in` here
---
*/

#line 1
extern(C) void wrongLink1 (in int a);
extern(Windows) void wrongLink2 (in void* a);
struct Large { ulong[64] data; }
extern(C) void wrongLink3 (in Large);
