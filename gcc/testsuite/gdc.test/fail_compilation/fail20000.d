/*
TEST_OUTPUT:
---
fail_compilation/fail20000.d(37): Error: cast from `fail20000.DClass` to `fail20000.CppClass` is not allowed in a `@safe` function
fail_compilation/fail20000.d(37):        Source object type is incompatible with target type
fail_compilation/fail20000.d(38): Error: cast from `fail20000.DInterface` to `fail20000.CppClass` is not allowed in a `@safe` function
fail_compilation/fail20000.d(38):        Source object type is incompatible with target type
fail_compilation/fail20000.d(39): Error: cast from `fail20000.CppClass2` to `fail20000.CppClass` is not allowed in a `@safe` function
fail_compilation/fail20000.d(39):        Source object type is incompatible with target type
fail_compilation/fail20000.d(40): Error: cast from `fail20000.CppInterface2` to `fail20000.CppClass` is not allowed in a `@safe` function
fail_compilation/fail20000.d(40):        Source object type is incompatible with target type
fail_compilation/fail20000.d(42): Error: cast from `fail20000.DClass` to `fail20000.CppInterface` is not allowed in a `@safe` function
fail_compilation/fail20000.d(42):        Source object type is incompatible with target type
fail_compilation/fail20000.d(43): Error: cast from `fail20000.DInterface` to `fail20000.CppInterface` is not allowed in a `@safe` function
fail_compilation/fail20000.d(43):        Source object type is incompatible with target type
fail_compilation/fail20000.d(44): Error: cast from `fail20000.CppClass2` to `fail20000.CppInterface` is not allowed in a `@safe` function
fail_compilation/fail20000.d(44):        Source object type is incompatible with target type
fail_compilation/fail20000.d(45): Error: cast from `fail20000.CppInterface2` to `fail20000.CppInterface` is not allowed in a `@safe` function
fail_compilation/fail20000.d(45):        Source object type is incompatible with target type
fail_compilation/fail20000.d(47): Error: cast from `fail20000.CppClass` to `fail20000.DClass` is not allowed in a `@safe` function
fail_compilation/fail20000.d(47):        Source object type is incompatible with target type
fail_compilation/fail20000.d(48): Error: cast from `fail20000.CppInterface` to `fail20000.DClass` is not allowed in a `@safe` function
fail_compilation/fail20000.d(48):        Source object type is incompatible with target type
fail_compilation/fail20000.d(50): Error: cast from `fail20000.CppClass` to `fail20000.DInterface` is not allowed in a `@safe` function
fail_compilation/fail20000.d(50):        Source object type is incompatible with target type
fail_compilation/fail20000.d(51): Error: cast from `fail20000.CppInterface` to `fail20000.DInterface` is not allowed in a `@safe` function
fail_compilation/fail20000.d(51):        Source object type is incompatible with target type
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
