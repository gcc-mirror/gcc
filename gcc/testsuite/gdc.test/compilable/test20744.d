// https://issues.dlang.org/show_bug.cgi?id=20744

struct A {
    struct S {}
    void f(@S int = 3);
    alias fun = Issue20744!f;
}

template Issue20744(func...) {
    static if (is(typeof(func[0]) PT == __parameters)) {
        alias Issue20744 = (PT args) {};
    }
}
