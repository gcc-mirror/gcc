/*
TEST_OUTPUT:
---
fail_compilation/fail23151.d(30): Error: class `fail23151.makeDerivedObj.Derived` is nested within `makeDerivedObj`, but super class `Base` is nested within `makeBaseObj`
---
*/
interface I
{
    void intfunc(int x);
}

auto makeBaseObj()
{
    int realPrivateX;
    class Base : I
    {
        private int modulePrivateX;
        int publicX;
        override void intfunc(int x)
        {
            realPrivateX++; // expected OK
        }
    }
    return new Base;
}

auto makeDerivedObj()
{
    int realPrivateY;
    class Derived : typeof(makeBaseObj())
    {
        private int modulePrivateY;
        int publicY;
        override void intfunc(int x)
        {
            realPrivateX++; // expected NG
            modulePrivateX++;
        }
    }
    return new Derived;
}
