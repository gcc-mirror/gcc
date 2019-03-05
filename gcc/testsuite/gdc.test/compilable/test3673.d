class Base {}

class Foo(T)
    if (is(T == int)) : Base { }

class Bar(T) : Base
    if (is(T == bool))
{ }

interface OutputRange(T...)
    if (T.length == 1)
{
    void put(T[0] value);
}

interface OutputRange(T...) : OutputRange!(T[0]), OutputRange!(T[1 .. $])
    if (T.length > 1)
{
}

alias OutputRange!(int, float) OR;

class COR : OR
{
    void put(int) { }
    void put(float) { }
}

class A {};
class B(T) : A if (true) {}
class C(T) if (false) : A {}

alias Foo!int FooInt;
alias Bar!bool BarBool;

static assert(!__traits(compiles, Foo!bool));
static assert(!__traits(compiles, Bar!int));

void main()
{
    auto fi = new FooInt;
    auto bb = new BarBool;
    auto cor = new COR;

    auto a = new A();
    auto b = new B!int();
    static assert(!__traits(compiles, new C!int()));
}
