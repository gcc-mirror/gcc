/*
TEST_OUTPUT:
---
fail_compilation/fail11375.d(18): Error: constructor `fail11375.D!().D.this` is not `nothrow`
       which calls `this`
fail_compilation/fail11375.d(16): Error: function `D main` may throw but is marked as `nothrow`
---
*/

class B {
    this() {}
}

class D() : B {}

void main() nothrow
{
    auto d = new D!()();
}
