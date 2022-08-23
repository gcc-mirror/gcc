/* TEST_OUTPUT:
---
fail_compilation/test17425.d(24): Error: parameter index must be in range 0..4 not 4
fail_compilation/test17425.d(27): Error: first argument to `__traits(getParameterStorageClasses, i, 4)` is not a function or a function call
fail_compilation/test17425.d(29): Error: expression expected as second argument of `__traits(getParameterStorageClasses, foo, int)`
fail_compilation/test17425.d(31): Error: expected 2 arguments for `getParameterStorageClasses` but had 3
---
*/

// https://issues.dlang.org/show_bug.cgi?id=17425

ref int foo(return ref const int* p, scope int* a, out int b, lazy int c);

//pragma(msg, __traits(getParameterStorageClasses, foo, 0));

static assert(__traits(getParameterStorageClasses, foo, 0)[0] == "return");
static assert(__traits(getParameterStorageClasses, foo, 0)[1] == "ref");

//pragma(msg, __traits(getParameterStorageClasses, foo, 1));
static assert(__traits(getParameterStorageClasses, foo, 1)[0] == "scope");
static assert(__traits(getParameterStorageClasses, foo, 2)[0] == "out");
static assert(__traits(getParameterStorageClasses, typeof(&foo), 3)[0] == "lazy");

enum a1 = __traits(getParameterStorageClasses, foo, 4);

int i;
enum a2 = __traits(getParameterStorageClasses, i, 4);

enum a3 = __traits(getParameterStorageClasses, foo, int);

enum a4 = __traits(getParameterStorageClasses, foo, 0, 1);
