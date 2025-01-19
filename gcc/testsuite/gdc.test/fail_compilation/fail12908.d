/*
TEST_OUTPUT:
---
fail_compilation/fail12908.d(14): Error: `pure` delegate `fail12908.main.__foreachbody_L12_C5` cannot call impure function `fail12908.g`
---
*/

void g() {}

void main() pure
{
    foreach (k, v; ["": ""])
    {
        g();
    }
}
