/* TEST_OUTPUT:
---
fail_compilation/test23786.d(22): Error: function `foo` is not callable using argument types `(double)`
fail_compilation/test23786.d(22):        cannot pass argument `1.0` of type `double` to parameter `int i`
fail_compilation/test23786.d(19):        `test23786.foo(int i)` declared here
fail_compilation/test23786.d(29): Error: function `bar` is not callable using argument types `(int*)`
fail_compilation/test23786.d(29):        cannot pass argument `& i` of type `int*` to parameter `int i`
fail_compilation/test23786.d(26):        `test23786.bar(int i)` declared here
fail_compilation/test23786.d(37): Error: function `baz` is not callable using argument types `(int*)`
fail_compilation/test23786.d(37):        cannot pass argument `& i` of type `int*` to parameter `int i`
fail_compilation/test23786.d(34):        `test23786.baz(int i)` declared here
---
*/

// https://issues.dlang.org/show_bug.cgi?id=23786

module test23786;

void foo(int i)
{
    static assert(__traits(parent, {}).mangleof == "_D9test237863fooFiZv");
    __traits(parent, {})(1.0);
}
void foo(int* p) {}

void bar(int i)
{
    static assert(__traits(parent, {}).mangleof == "_D9test237863barFiZv");
    __traits(parent, {})(&i);
}
void bar(int* p) {}

void baz(int* p) {}
void baz(int i)
{
    static assert(__traits(parent, {}).mangleof == "_D9test237863bazFiZv");
    __traits(parent, {})(&i);
}
void baz(float* p) {}
