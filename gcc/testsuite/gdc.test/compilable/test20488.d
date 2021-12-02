module test20488;

// https://issues.dlang.org/show_bug.cgi?id=20488
struct Bar {
    void opDispatch(string s, Args...) (Args args) {
    }
    void fun() {
        (bool[int]).init.length;
        this.f((int[int]).init.length);
    }
}
