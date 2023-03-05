/*
REQUIRED_ARGS: -lowmem -Jrunnable -preview=rvaluerefparam
EXTRA_FILES: xtest46.d
TEST_OUTPUT:
---
runnable/xtest46_gc.d-mixin-33(197): Deprecation: alias this for classes/interfaces is deprecated
Boo!double
Boo!int
true
int
!! immutable(int)[]
runnable/xtest46_gc.d-mixin-33(2964): Deprecation: alias this for classes/interfaces is deprecated
runnable/xtest46_gc.d-mixin-33(2996): Deprecation: alias this for classes/interfaces is deprecated
int(int i, long j = 7L)
long
C10390(C10390(<recursion>))
tuple(height)
tuple(get, get)
tuple(clear)
tuple(draw, draw)
const(int)
string[]
double[]
double[]
{}
runnable/xtest46_gc.d-mixin-33(4702): Deprecation: alias this for classes/interfaces is deprecated
tuple("m")
true
TFunction1: extern (C) void function()
---
*/

mixin(import("xtest46.d"));
