/*
TEST_OUTPUT:
---
fail_compilation/fail11510.d(25): Error: reinterpretation through overlapped field `y` is not allowed in CTFE
fail_compilation/fail11510.d(29):        called from here: `test11510a()`
fail_compilation/fail11510.d(36): Error: reinterpretation through overlapped field `y` is not allowed in CTFE
fail_compilation/fail11510.d(40):        called from here: `test11510b()`
---
*/

struct S11510
{
    union
    {
        size_t x;
        int* y; // pointer field
    }
}

bool test11510a()
{
    S11510 s;

    s.y = [1,2,3].ptr;
    auto x = s.x;   // reinterpretation

    return true;
}
enum a = test11510a();

bool test11510b()
{
    S11510 s;

    s.x = 10;
    auto y = s.y;   // reinterpretation

    return true;
}
enum b = test11510b();
