/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/deprecations_preview_in.d(1): Deprecation: using `in` parameters with `extern(C)` functions is deprecated
fail_compilation/deprecations_preview_in.d(1):        parameter `__anonymous_param` declared as `in` here
---
*/

#line 1
extern(C) void fun1(in char*);
