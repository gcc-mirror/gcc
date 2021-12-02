/* TEST_OUTPUT:
---
Foo
Bar
Foo
Bar
Bar
Foo
Bar
---
*/

// https://issues.dlang.org/show_bug.cgi?id=6400

enum int base(string name) = 10 * (name[$-1] - '0');
struct Foo { int opDispatch(string name)() { pragma(msg, "Foo"); return base!name + 1; } }
struct Bar { int opDispatch(string name)() { pragma(msg, "Bar"); return base!name + 2; } }
struct Baz {  }

void main()
{
    assert(test());
    static assert(test());
}

bool test()
{
    auto foo = new Foo;
    auto bar = new Bar;
    auto baz = new Baz;

    with (foo)
    {
        assert(f1() == 11);
        with (baz) assert(f1() == 11);
        with (bar)
        {
            assert(f2() == 22);
            with (baz) assert(f2() == 22);
            with (foo)
            {
                assert(f3() == 31);
                with (baz) assert(f3() == 31);
                with (bar)
                {
                    assert(f4() == 42);
                    with (baz) assert(f4() == 42);
                    with (baz)
                    {
                        assert(f5() == 52);
                        with (baz) assert(f5() == 52);
                    }
                    with (foo)
                    {
                        assert(f6() == 61);
                        with (baz) assert(f6() == 61);
                    }
                    with (bar)
                    {
                        assert(f7() == 72);
                        with (baz) assert(f7() == 72);
                    }
                }
            }
        }
    }

    return true;
}
