/*
TEST_OUTPUT:
---
fail_compilation/test23017.d(16): Error: class `test23017.CppChildA` with C++ linkage cannot inherit from class `DClass` with D linkage
---
*/

// https://issues.dlang.org/show_bug.cgi?id=23017
// C++ class may not derive from D class
extern(D) class DClass {}
extern(C++) class CppClass
{
    void foo();
}

extern(C++) class CppChildA : DClass {} // error
extern(C++) class CppChildB : CppClass {}

extern(D) class DChildA : DClass {}
extern(D) class DChildB : CppClass {} // automatically made extern(C++)
