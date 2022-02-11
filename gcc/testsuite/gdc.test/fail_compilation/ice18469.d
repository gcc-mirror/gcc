/*
TEST_OUTPUT:
---
fail_compilation/ice18469.d(10): Error: no property `opCall` for type `void`
---
*/
class Bar
{
    ~this(){}
    this(){alias T = typeof(Bar.__dtor.opCall);}
}

void main() {}
