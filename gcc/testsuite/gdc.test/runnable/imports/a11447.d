module imports.a11447;
struct A { }

void map(alias dg)(A r) { }

struct TTT
{
    static auto yyy(A a)
    {
        map!(b => 0)(a);
    }
}

void bar() { }
