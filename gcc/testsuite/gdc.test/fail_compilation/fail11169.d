/*
TEST_OUTPUT:
---
fail_compilation/fail11169.d(16): Error: error evaluating static if expression
---
*/

class A
{
    abstract void foo();
}

class B : A
{
    // __traits(isAbstractClass) is not usable in static if condition.
    static if (__traits(isAbstractClass, typeof(this)))
    {
    }

    override void foo()
    {
    }
}

void main()
{
    B b = new B();
}
