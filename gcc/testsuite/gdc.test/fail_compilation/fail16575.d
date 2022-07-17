// https://issues.dlang.org/show_bug.cgi?id=16575
/*
REQUIRED_ARGS: -m64
TEST_OUTPUT:
---
fail_compilation/fail16575.d(10): Error: function `fail16575.immNull` cannot have parameter of type `immutable(typeof(null))*` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(11): Error: function `fail16575.shaNull` cannot have parameter of type `shared(typeof(null))*` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(20): Error: function `fail16575.immNoReturn` cannot have parameter of type `immutable(noreturn)*` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(21): Error: function `fail16575.shaNoReturn` cannot have parameter of type `shared(noreturn)*` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(30): Error: function `fail16575.immBasic` cannot have parameter of type `immutable(int)*` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(31): Error: function `fail16575.shaBasic` cannot have parameter of type `shared(int)*` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(40): Error: function `fail16575.immVector` cannot have parameter of type `immutable(__vector(long[2]))*` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(41): Error: function `fail16575.shaVector` cannot have parameter of type `shared(__vector(long[2]))*` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(50): Error: function `fail16575.immSArray` cannot have parameter of type `immutable(long[2])` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(50):        perhaps use a `long*` type instead
fail_compilation/fail16575.d(51): Error: function `fail16575.shaSArray` cannot have parameter of type `shared(long[2])` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(51):        perhaps use a `long*` type instead
fail_compilation/fail16575.d(60): Error: function `fail16575.immPointer` cannot have parameter of type `immutable(int*)` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(61): Error: function `fail16575.shaPointer` cannot have parameter of type `shared(int*)` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(71): Error: function `fail16575.immStruct` cannot have parameter of type `immutable(SPP)*` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(72): Error: function `fail16575.shaStruct` cannot have parameter of type `shared(SPP)*` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(81): Error: function `fail16575.immClass` cannot have parameter of type `immutable(CPP)` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(82): Error: function `fail16575.shaClass` cannot have parameter of type `shared(CPP)` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(91): Error: function `fail16575.immEnum` cannot have parameter of type `immutable(EPP)*` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(92): Error: function `fail16575.shaEnum` cannot have parameter of type `shared(EPP)*` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(100): Error: function `fail16575.typeDArray` cannot have parameter of type `int[]` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(101): Error: function `fail16575.typeAArray` cannot have parameter of type `int[int]` because its linkage is `extern(C++)`
fail_compilation/fail16575.d(102): Error: function `fail16575.typeDelegate` cannot have parameter of type `extern (C++) int delegate()` because its linkage is `extern(C++)`
---
*/

#line 10
extern(C++) void immNull(immutable(typeof(null))* a) {}
extern(C++) void shaNull(shared(typeof(null))* a) {}
#line 20
extern(C++) void immNoReturn(immutable(typeof(*null))* a) {}
extern(C++) void shaNoReturn(shared(typeof(*null))* a) {}
#line 30
extern(C++) void immBasic(immutable(int)* a) {}
extern(C++) void shaBasic(shared(int)* a) {}
#line 40
extern(C++) void immVector(immutable(__vector(long[2]))* a) {}
extern(C++) void shaVector(shared(__vector(long[2]))* a) {}
#line 50
extern(C++) void immSArray(immutable(long[2]) a) {}
extern(C++) void shaSArray(shared(long[2]) a) {}
#line 60
extern(C++) void immPointer(immutable(int*) a) {}
extern(C++) void shaPointer(shared(int*) a) {}
#line 70
extern(C++) struct SPP {}
extern(C++) void immStruct(immutable(SPP)* a) {}
extern(C++) void shaStruct(shared(SPP)* a) {}
#line 80
extern(C++) class CPP {}
extern(C++) void immClass(immutable CPP a) {}
extern(C++) void shaClass(shared CPP a) {}
#line 90
extern(C++) enum EPP {a}
extern(C++) void immEnum(immutable(EPP)* a) {}
extern(C++) void shaEnum(shared(EPP)* a) {}
# line 100
extern(C++) void typeDArray(int[] a) {}
extern(C++) void typeAArray(int[int] a) {}
extern(C++) void typeDelegate(int delegate() a) {}
