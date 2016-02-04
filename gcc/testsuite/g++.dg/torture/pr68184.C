// { dg-do run }
namespace {
struct IFoo { virtual void foo() = 0; };
struct IBar { virtual void bar() = 0; };

struct FooBar : private IBar, private IFoo
{
    void call_foo()
    {
        try
        {
            static_cast<IFoo*>(this)->foo();
        }
        catch( ... ) {}
    }
    void foo() { throw 1; }
    void bar()  {}
};

void test()
{
    FooBar foobar;
    foobar.call_foo();
}
}
int main()
{
    test();
    return 0;
}

