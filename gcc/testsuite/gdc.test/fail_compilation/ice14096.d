/*
TEST_OUTPUT:
---
fail_compilation/ice14096.d(29): Error: cannot access frame pointer of `ice14096.main.Baz!((i) => i).Baz`
fail_compilation/ice14096.d(23): Error: template instance `ice14096.foo!(Tuple!(Baz!((i) => i))).foo.bar!(t)` error instantiating
fail_compilation/ice14096.d(40):        instantiated from here: `foo!(Tuple!(Baz!((i) => i)))`
---
*/

struct Tuple(Types...)
{
    Types expand;
    alias expand this;
    alias field = expand;
}
Tuple!T tuple(T...)(T args)
{
    return typeof(return)(args);
}

auto foo(T)(T t)
{
    bar!t();
}

auto bar(alias s)()
{
    // default construction is not possible for: Tuple!(Baz!(i => i))
    typeof(s) p;
}

struct Baz(alias f)
{
    void g() {}
}

void main()
{
    auto t = tuple(Baz!(i => i)());
    foo(t);
}
