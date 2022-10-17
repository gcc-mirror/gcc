/*
TEST_OUTPUT:
---
fail_compilation/fail20000.d(25): Error: cast from `fail20000.DClass` to `fail20000.CppClass` not allowed in safe code
fail_compilation/fail20000.d(26): Error: cast from `fail20000.DInterface` to `fail20000.CppClass` not allowed in safe code
fail_compilation/fail20000.d(27): Error: cast from `fail20000.CppClass2` to `fail20000.CppClass` not allowed in safe code
fail_compilation/fail20000.d(28): Error: cast from `fail20000.CppInterface2` to `fail20000.CppClass` not allowed in safe code
fail_compilation/fail20000.d(30): Error: cast from `fail20000.DClass` to `fail20000.CppInterface` not allowed in safe code
fail_compilation/fail20000.d(31): Error: cast from `fail20000.DInterface` to `fail20000.CppInterface` not allowed in safe code
fail_compilation/fail20000.d(32): Error: cast from `fail20000.CppClass2` to `fail20000.CppInterface` not allowed in safe code
fail_compilation/fail20000.d(33): Error: cast from `fail20000.CppInterface2` to `fail20000.CppInterface` not allowed in safe code
fail_compilation/fail20000.d(35): Error: cast from `fail20000.CppClass` to `fail20000.DClass` not allowed in safe code
fail_compilation/fail20000.d(36): Error: cast from `fail20000.CppInterface` to `fail20000.DClass` not allowed in safe code
fail_compilation/fail20000.d(38): Error: cast from `fail20000.CppClass` to `fail20000.DInterface` not allowed in safe code
fail_compilation/fail20000.d(39): Error: cast from `fail20000.CppInterface` to `fail20000.DInterface` not allowed in safe code
---
*/
extern(C++) class CppClass { int a; }
extern(C++) class CppClass2 { void* a; }
extern(C++) interface CppInterface { int b(); }
extern(C++) interface CppInterface2 { void* b(); }
class DClass { int c; }
interface DInterface { int d(); }

bool isCppClass(DClass a) @safe { return cast(CppClass) a !is null; }
bool isCppClass(DInterface a) @safe { return cast(CppClass) a !is null; }
bool isCppClass(CppClass2 a) @safe { return cast(CppClass) a !is null; }
bool isCppClass(CppInterface2 a) @safe { return cast(CppClass) a !is null; }

bool isCppInterface(DClass a) @safe { return cast(CppInterface) a !is null; }
bool isCppInterface(DInterface a) @safe { return cast(CppInterface) a !is null; }
bool isCppInterface(CppClass2 a) @safe { return cast(CppInterface) a !is null; }
bool isCppInterface(CppInterface2 a) @safe { return cast(CppInterface) a !is null; }

bool isDClass(CppClass a) @safe { return cast(DClass) a !is null; }
bool isDClass(CppInterface a) @safe { return cast(DClass) a !is null; }

bool isDInterface(CppClass a) @safe { return cast(DInterface) a !is null; }
bool isDInterface(CppInterface a) @safe { return cast(DInterface) a !is null; }
