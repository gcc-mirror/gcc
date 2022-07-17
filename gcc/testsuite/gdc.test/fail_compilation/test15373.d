/*
TEST_OUTPUT:
---
fail_compilation/test15373.d(21): Error: runtime type information is not supported for `extern(C++)` classes
---
*/

// https://issues.dlang.org/show_bug.cgi?id=15373

// Using `typeid` on an `extern(C++) class` type is ok  as it is evaluated at compile-time
// See test/runnable/test15373.d

// Using `typeid` on an `extern(C++) class` instance is not ok because `extern(C++) class`
// instances are not rooted in `Object`

extern(C++) class C { }

void foo()
{
    C c = new C();
    auto ti = typeid(c);
}
