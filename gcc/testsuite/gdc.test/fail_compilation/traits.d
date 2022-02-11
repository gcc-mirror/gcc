/************************************************************/

/*
TEST_OUTPUT:
---
fail_compilation/traits.d(100): Error: `getTargetInfo` key `"not_a_target_info"` not supported by this implementation
fail_compilation/traits.d(101): Error: string expected as argument of __traits `getTargetInfo` instead of `100`
fail_compilation/traits.d(102): Error: expected 1 arguments for `getTargetInfo` but had 2
fail_compilation/traits.d(103): Error: expected 1 arguments for `getTargetInfo` but had 0
fail_compilation/traits.d(200): Error: undefined identifier `imports.nonexistent`
fail_compilation/traits.d(201): Error: undefined identifier `imports.nonexistent`
fail_compilation/traits.d(202): Error: expected 1 arguments for `isPackage` but had 0
fail_compilation/traits.d(203): Error: expected 1 arguments for `isModule` but had 0
fail_compilation/traits.d(300): Error: In expression `__traits(allMembers, float)` `float` can't have members
fail_compilation/traits.d(300):        `float` must evaluate to either a module, a struct, an union, a class, an interface or a template instantiation
fail_compilation/traits.d(306): Error: In expression `__traits(allMembers, TemplatedStruct)` struct `TemplatedStruct(T)` has no members
fail_compilation/traits.d(306):        `TemplatedStruct(T)` must evaluate to either a module, a struct, an union, a class, an interface or a template instantiation
fail_compilation/traits.d(309): Error: In expression `__traits(derivedMembers, float)` `float` can't have members
fail_compilation/traits.d(309):        `float` must evaluate to either a module, a struct, an union, a class, an interface or a template instantiation
fail_compilation/traits.d(316): Error: In expression `__traits(derivedMembers, TemplatedStruct)` struct `TemplatedStruct(T)` has no members
fail_compilation/traits.d(316):        `TemplatedStruct(T)` must evaluate to either a module, a struct, an union, a class, an interface or a template instantiation
fail_compilation/traits.d(404): Error: function `traits.func1` circular reference in `__traits(GetCppNamespaces,...)`
fail_compilation/traits.d(413): Error: function `traits.foo1.func1` circular reference in `__traits(GetCppNamespaces,...)`
---
*/

#line 100
enum A1 = __traits(getTargetInfo, "not_a_target_info");
enum B1 = __traits(getTargetInfo, 100);
enum C1 = __traits(getTargetInfo, "cppRuntimeLibrary", "bits");
enum D1 = __traits(getTargetInfo);

#line 200
enum A2 = __traits(isPackage, imports.nonexistent);
enum B2 = __traits(isModule, imports.nonexistent);
enum C2 = __traits(isPackage);
enum D2 = __traits(isModule);

interface Interface {}
struct TemplatedStruct(T) {}
struct Struct {}
union Union {}
class Class {}

#line 300
enum AM0 = __traits(allMembers, float);                     // compile error
enum AM1 = __traits(allMembers, Struct);                    // no error
enum AM2 = __traits(allMembers, Union);                     // no error
enum AM3 = __traits(allMembers, Class);                     // no error
enum AM4 = __traits(allMembers, Interface);                 // no error
enum AM5 = __traits(allMembers, TemplatedStruct!float);     // no error
enum AM6 = __traits(allMembers, TemplatedStruct);           // compile error
enum AM7 = __traits(allMembers, mixin(__MODULE__));         // no error

enum DM0 = __traits(derivedMembers, float);                 // compile error
enum DM1 = __traits(derivedMembers, Struct);                // no error
enum DM2 = __traits(derivedMembers, Struct);                // no error
enum DM3 = __traits(derivedMembers, Union);                 // no error
enum DM4 = __traits(derivedMembers, Class);                 // no error
enum DM5 = __traits(derivedMembers, Interface);             // no error
enum DM6 = __traits(derivedMembers, TemplatedStruct!float); // no error
enum DM7 = __traits(derivedMembers, TemplatedStruct);       // compile error
enum DM8 = __traits(derivedMembers, mixin(__MODULE__));     // no error

#line 400
extern(C++, "bar")
extern(C++, __traits(getCppNamespaces, func1)) void func () {}

extern(C++, "foo")
extern(C++, __traits(getCppNamespaces, func2)) void func1 () {}

extern(C++, "foobar")
extern(C++, __traits(getCppNamespaces, func)) void func2 () {}

extern(C++, bar1)
extern(C++, __traits(getCppNamespaces, foo1.func1)) void func () {}

extern(C++, foo1)
extern(C++, __traits(getCppNamespaces, foobar1.func2)) void func1 () {}

extern(C++, foobar1)
extern(C++, __traits(getCppNamespaces, bar1.func)) void func2 () {}

/********************************************
https://issues.dlang.org/show_bug.cgi?id=21918

TEST_OUTPUT:
---
fail_compilation/traits.d(501): Error: undefined identifier `T`
fail_compilation/traits.d(502):        while evaluating `pragma(msg, __traits(getParameterStorageClasses, yip, 0))`
---
*/
#line 500

auto yip(int f) {return T[];}
pragma(msg, __traits(getParameterStorageClasses, yip, 0));


/********************************************
TEST_OUTPUT:
---
fail_compilation/traits.d(602): Error: expected 1 arguments for `hasCopyConstructor` but had 0
fail_compilation/traits.d(602):        while evaluating `pragma(msg, __traits(hasCopyConstructor))`
fail_compilation/traits.d(603): Error: type expected as second argument of __traits `hasCopyConstructor` instead of `S()`
fail_compilation/traits.d(603):        while evaluating `pragma(msg, __traits(hasCopyConstructor, S()))`
fail_compilation/traits.d(604): Error: expected 1 arguments for `hasPostblit` but had 0
fail_compilation/traits.d(604):        while evaluating `pragma(msg, __traits(hasPostblit))`
fail_compilation/traits.d(605): Error: type expected as second argument of __traits `hasPostblit` instead of `S()`
fail_compilation/traits.d(605):        while evaluating `pragma(msg, __traits(hasPostblit, S()))`
---
*/
#line 600

struct S { this (ref S rhs) {} }
pragma(msg, __traits(hasCopyConstructor));
pragma(msg, __traits(hasCopyConstructor, S()));
pragma(msg, __traits(hasPostblit));
pragma(msg, __traits(hasPostblit, S()));
