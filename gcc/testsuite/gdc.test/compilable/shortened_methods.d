// N.B. Shortened methods are no longer under a preview flag
class A {
    int _x = 34;
    // short syntax works in all contexts
    @property x() => _x;
    @property x(int v) => _x = v;

    // including with contracts
    @property y() in(true) => _x;

    // or other auto returns
    auto foo() @safe => assert(0);

    // or normal method defintions
    bool isNull() => this is null;
}

class B : A{
    // short syntax also overrides the same as long syntax
    override bool isNull() => this !is null;
}

static assert((new A).x == 34);

string test() => "hello"; // works at any scope

static assert(test() == "hello"); // works normally
static assert(is(typeof(&test) == string function())); // same normal type

void func() {
    int a;
    int nested() => a; // and at nested scopes too
}
