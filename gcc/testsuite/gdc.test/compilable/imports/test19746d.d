import test19746;
class Baz(T): Foo { }
class Dap(T): Baz!T
{
    override void thunk() {}
}
class Zoo
{
    Dap!int _dap;
}
