// REQUIRED_ARGS: -unittest
module issue19925;

unittest {
    with (S) {
        a(); // Compiles!
        b(); // Fails!
    }
}

struct S {
    static void a() {}
    static void opDispatch(string name)() {}
}
