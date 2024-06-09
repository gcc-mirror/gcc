/*
TEST_OUTPUT:
---
fail_compilation/ice11518.d(17): Error: class `ice11518.B` matches more than one template declaration:
fail_compilation/ice11518.d(12):        `B(T : A!T)`
and:
fail_compilation/ice11518.d(13):        `B(T : A!T)`
---
*/

class A(T) {}
class B(T : A!T) {}
class B(T : A!T) {}

void main()
{
    new B!(A!void);
}
