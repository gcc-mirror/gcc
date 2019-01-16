/*
TEST_OUTPUT:
---
fail_compilation/diag8787.d(10): Error: function diag8787.I.f function body only allowed in final functions in interface I
---
*/

interface I
{
    void f() { }
}

void main() {}
