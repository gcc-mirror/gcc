/* TEST_OUTPUT:
---
Foo
Bar
---
*/
class Foo
{
    void opDispatch(string name)() { pragma(msg, "Foo"); }
}
class Bar
{
    void opDispatch(string name)() { pragma(msg, "Bar"); }
}
class Baz
{
}

void main()
{
    auto foo = new Foo;
    auto bar = new Bar;
    auto baz = new Baz;

    with (foo)
    {
        f0();
        with (bar)
        {
            f1();
        }
        with (baz)
        {
            static assert(!__traits(compiles, f2()));
        }
    }
}
