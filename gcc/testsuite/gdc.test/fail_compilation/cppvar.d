/*
TEST_OUTPUT:
---
fail_compilation/cppvar.d(10): Error: variable `cppvar.funcLiteral` cannot have `extern(C++)` linkage because it is `static`
fail_compilation/cppvar.d(10):        perhaps declare it as `__gshared` instead
fail_compilation/cppvar.d(20): Error: variable `cppvar.threadLocalVar` cannot have `extern(C++)` linkage because it is `static`
fail_compilation/cppvar.d(20):        perhaps declare it as `__gshared` instead
fail_compilation/cppvar.d(21): Error: variable `cppvar.staticVar` cannot have `extern(C++)` linkage because it is `static`
fail_compilation/cppvar.d(21):        perhaps declare it as `__gshared` instead
fail_compilation/cppvar.d(22): Error: variable `cppvar.sharedVar` cannot have `extern(C++)` linkage because it is `shared`
fail_compilation/cppvar.d(22):        perhaps declare it as `__gshared` instead
fail_compilation/cppvar.d(30): Error: delegate `cppvar.__lambda7` cannot return type `bool[3]` because its linkage is `extern(C++)`
---
*/
#line 10
extern(C++) bool[3] funcLiteral = () { bool[3] a; return a; };
#line 20
extern(C++) int threadLocalVar;
extern(C++) static int staticVar;
extern(C++) shared int sharedVar;
#line 30
extern(C++) __gshared bool[3] gfuncLiteral = () { bool[3] a; return a; };
