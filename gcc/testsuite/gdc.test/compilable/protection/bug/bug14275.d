module protection.bug.bug14275;

import protection.aggregate.mod14275;

// https://issues.dlang.org/show_bug.cgi?id=14275

void main() {
    Foo f;
    f.foo();
    static assert (!is(typeof(f.foo2())));
    bar();
}
