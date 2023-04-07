// https://issues.dlang.org/show_bug.cgi?id=23760

/*
TEST_OUTPUT:
---
fail_compilation/fail23760.d(16): Error: type of variable `fail23760.A.state` has errors
fail_compilation/fail23760.d(16): Error: `(A).state` cannot be resolved
fail_compilation/fail23760.d(21): Error: template instance `fail23760.JavaBridge!(A)` error instantiating
fail_compilation/fail23760.d(24):        instantiated from here: `JavaClass!(A)`
---
*/

class JavaBridge(Class)
{
    static if(is(typeof(__traits(getMember, Class, "state")))) {}
    alias T = __traits(getOverloads, Class, "state");
}

class JavaClass(CRTP)
{
    JavaBridge!(CRTP) _javaDBridge;
}

class A : JavaClass!A
{
    State* state;
}
