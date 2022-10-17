/* REQUIRED_ARGS: -betterC
 */

// https://issues.dlang.org/show_bug.cgi?id=18905

extern (C++) class C { } // Error: TypeInfo cannot be used with -betterC
