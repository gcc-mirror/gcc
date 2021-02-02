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
