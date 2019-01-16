class Base
{
    ~this() {}
    size_t x = 4;
}

interface Interface
{
    int Method();
}

class Derived : Base, Interface
{
    size_t y = 5;
    int Method() { return 3; }
}

static assert(Derived.x.offsetof == (void*).sizeof * 2);
static assert(Derived.y.offsetof == (void*).sizeof * 4);
