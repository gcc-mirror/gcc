/*
TEST_OUTPUT:
---
fail_compilation/test13537.d(31): Error: modifying field `U.y` which overlaps with fields with other storage classes is not allowed in a `@safe` function
fail_compilation/test13537.d(32): Error: modifying field `U.y` which overlaps with fields with other storage classes is not allowed in a `@safe` function
fail_compilation/test13537.d(33): Error: accessing overlapped field `U.z` with pointers is not allowed in a `@safe` function
fail_compilation/test13537.d(34): Error: modifying field `U.y` which overlaps with fields with other storage classes is not allowed in a `@safe` function
---
*/

// https://issues.dlang.org/show_bug.cgi?id=13537

union U
{
    immutable int x;
    int y;
    int* z;
}

union V
{
    immutable int x;
    const int y;
}

void fun() @safe
{
    U u;

    // errors
    u.y = 1;
    int* p = &u.y;
    int** q = &u.z;
    abc(u.y);

    // read access is allowed
    int a = u.x;
    a = u.y;
    def(u.y);

    // Overlapping const/immutable is allowed
    auto v = V(1);
    assert(v.y == 1);
}

void gun() @system
{
    U u;

    // allowed because system code
    u.y = 1;
    int* p = &u.y;
    int** q = &u.z;
    abc(u.y);
}

@safe:
void abc(ref int x) { }
void def(const ref int x) { }
