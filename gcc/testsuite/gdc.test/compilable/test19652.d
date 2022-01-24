struct Base {
    int i;
}

struct A {
    Base base;
    alias base this;
}

struct B {
    A a;
    alias a this;
}

auto asGeneric(inout ref Base block) @nogc {
    return &block;
}

B* thingie;
auto foo() {
    return asGeneric(*thingie);
}
