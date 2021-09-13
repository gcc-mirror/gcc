/*
TEST_OUTPUT:
---
fail_compilation/ice8604.d(9): Error: undefined identifier `i`
---
*/
struct StructFoo
{
    static if(i) { }
    else enum z = "";
}

void main() { }
