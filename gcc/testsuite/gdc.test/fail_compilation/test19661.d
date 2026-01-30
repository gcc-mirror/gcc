/*
EXTRA_FILES: imports/imp19661.d
TEST_OUTPUT:
---
fail_compilation/test19661.d(12): Error: variables cannot be initialized with an expression of type `void`
fail_compilation/test19661.d(12):        only `= void;` is allowed, which prevents default initialization
---
*/

module ice19661;

immutable bool testModule = testFunctionMembers!();

void testFunctionMembers()() {
    import imports.imp19661 : isFunction;
    foreach(member; __traits(allMembers, ice19661)) {
        bool b = isFunction!(__traits(getMember, ice19661, member));
    }
}
