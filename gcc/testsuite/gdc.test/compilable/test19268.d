/* REQUIRED_ARGS: -betterC
 */

// https://issues.dlang.org/show_bug.cgi?id=19268

mixin(`void foo(){}`.idup);
