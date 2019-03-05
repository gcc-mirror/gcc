/*
TEST_OUTPUT:
---
fail_compilation/fail340.d(18): Error: variable fail340.w of type struct const(CopyTest) uses this(this), which is not allowed in static initialization
fail_compilation/fail340.d(19):        while evaluating: `static assert(w.x == 55.0000)`
---
*/

struct CopyTest
{
    double x;
    this(double a) { x = a * 10.0;}
    this(this) { x += 2.0; }
}

const CopyTest z = CopyTest(5.3);

const CopyTest w = z;
static assert(w.x == 55.0);
