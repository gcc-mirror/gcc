module imports.link13394a;

class A
{
    this() { }
}

class Btpl(T) : T
{
    this()() { }

    invariant() {}
}

alias B = Btpl!A;

struct Stpl()
{
    void func()() {}

    invariant() {}
}

alias S = Stpl!();
