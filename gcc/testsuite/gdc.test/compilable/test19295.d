struct S1(T...) {
    auto fun() {
        static assert(__traits(compiles, &T[0]));
    }
}

struct S2 {
    void gun() {}
    S1!gun overloaded;
}
