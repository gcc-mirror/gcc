/*
REQUIRED_ARGS: -lowmem -Jrunnable -preview=rvaluerefparam
EXTRA_FILES: xtest46.d
TEST_OUTPUT:
---
Boo!double
Boo!int
true
int
!! immutable(int)[]
int(int i, long j = 7L)
long
C10390(C10390(<recursion>))
tuple(height)
tuple(get, get)
tuple(clear)
tuple(draw, draw)
runnable/xtest46_gc.d-mixin-$n$(185): Deprecation: `opDot` is deprecated. Use `alias this`
runnable/xtest46_gc.d-mixin-$n$(187): Deprecation: `opDot` is deprecated. Use `alias this`
runnable/xtest46_gc.d-mixin-$n$(188): Deprecation: `opDot` is deprecated. Use `alias this`
runnable/xtest46_gc.d-mixin-$n$(190): Deprecation: `opDot` is deprecated. Use `alias this`
runnable/xtest46_gc.d-mixin-$n$(217): Deprecation: `opDot` is deprecated. Use `alias this`
runnable/xtest46_gc.d-mixin-$n$(219): Deprecation: `opDot` is deprecated. Use `alias this`
runnable/xtest46_gc.d-mixin-$n$(220): Deprecation: `opDot` is deprecated. Use `alias this`
runnable/xtest46_gc.d-mixin-$n$(222): Deprecation: `opDot` is deprecated. Use `alias this`
const(int)
string[]
double[]
double[]
{}
tuple("m")
true
TFunction1: extern (C) void function()
---
*/

mixin(import("xtest46.d"));
