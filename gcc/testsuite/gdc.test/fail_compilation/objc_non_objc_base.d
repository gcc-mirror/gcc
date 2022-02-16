// EXTRA_OBJC_SOURCES:
/*
TEST_OUTPUT:
---
fail_compilation/objc_non_objc_base.d(12): Error: class `objc_non_objc_base.A` base class for an Objective-C class must be `extern (Objective-C)`
---
*/

interface Base {}

extern (Objective-C)
class A : Base {}
