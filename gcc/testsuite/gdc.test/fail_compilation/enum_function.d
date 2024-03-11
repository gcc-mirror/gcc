/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/enum_function.d(11): Deprecation: function cannot have enum storage class
fail_compilation/enum_function.d(12): Deprecation: function cannot have enum storage class
fail_compilation/enum_function.d(13): Deprecation: function cannot have enum storage class
fail_compilation/enum_function.d(14): Deprecation: function cannot have enum storage class
---
*/
enum void f1() { return; }
enum f2() { return 5; }
enum f3() => 5;
enum int f4()() => 5;
