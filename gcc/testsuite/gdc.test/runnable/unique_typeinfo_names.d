module unique_typeinfo_names;

// https://issues.dlang.org/show_bug.cgi?id=22149
void structs()
{
    static struct Foo(T) {}

    auto foo()
    {
        struct S {}
        return Foo!S();
    }

    auto bar()
    {
        struct S {}
        return Foo!S();
    }

    auto f = foo();
    auto b = bar();

    assert(typeid(f) != typeid(b));
    assert(typeid(f).name != typeid(b).name);

    assert(typeid(f).mangledName == typeof(f).mangleof);
    assert(typeid(b).mangledName == typeof(b).mangleof);
    assert(typeid(f).name == "unique_typeinfo_names.structs().Foo!(unique_typeinfo_names.structs().foo().S).Foo");
    assert(typeid(b).name == "unique_typeinfo_names.structs().Foo!(unique_typeinfo_names.structs().bar().S).Foo");
}

// https://issues.dlang.org/show_bug.cgi?id=22150
void classes()
{
    static class Foo(T) {}

    static auto foo()
    {
        struct S {}
        return new Foo!S();
    }

    static auto bar()
    {
        struct S {}
        return new Foo!S();
    }

    auto f = foo();
    auto b = bar();

    assert(typeid(f) != typeid(b));
    assert(typeid(f).name != typeid(b).name);

    assert(typeid(f).name == "unique_typeinfo_names.classes.Foo!(unique_typeinfo_names.classes.foo.S).Foo");
    assert(typeid(b).name == "unique_typeinfo_names.classes.Foo!(unique_typeinfo_names.classes.bar.S).Foo");
}

void interfaces()
{
    static interface IFoo(T) {}
    static class Foo(T) : IFoo!T {}

    static auto foo()
    {
        struct S {}
        IFoo!S r = new Foo!S();
        return r;
    }

    static auto bar()
    {
        struct S {}
        IFoo!S r = new Foo!S();
        return r;
    }

    auto f = foo();
    auto b = bar();

    assert(typeid(f) != typeid(b));
    assert(typeid(f).name != typeid(b).name);

    assert(typeid(f).name == "unique_typeinfo_names.interfaces.IFoo!(unique_typeinfo_names.interfaces.foo.S).IFoo");
    assert(typeid(b).name == "unique_typeinfo_names.interfaces.IFoo!(unique_typeinfo_names.interfaces.bar.S).IFoo");
}

void main()
{
    structs();
    classes();
    interfaces();
}
