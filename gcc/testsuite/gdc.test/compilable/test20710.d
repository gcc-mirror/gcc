// https://issues.dlang.org/show_bug.cgi?id=20710

// with empty array literal
immutable A a = A(B([]));
immutable B b = a.b;

struct A {
    B b;
}

struct B {
    int[] c;
}

// with empty struct literal
immutable C c = C(D());
immutable D d = c.d;

struct C {
    D d;
}

struct D {
}
