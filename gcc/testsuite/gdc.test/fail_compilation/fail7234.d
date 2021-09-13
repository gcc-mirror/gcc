/*
TEST_OUTPUT:
---
fail_compilation/fail7234.d(14): Error: no property `empty` for type `Contract*`, perhaps `import std.range;` is needed?
---
*/

struct Contract {
    void opDispatch()(){}
}

void foo()
{
    Contract* r; if (r.empty) {}
}

