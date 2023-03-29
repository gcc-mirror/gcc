// https://issues.dlang.org/show_bug.cgi?id=23497

class A {}

A getA(T t) {
    return t.a;
}

struct T {
    A _a;

    void k() {}

    auto a() in {
        k();
    } do {
        return _a;
    }
}
