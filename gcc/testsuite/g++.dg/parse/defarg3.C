struct A {
    enum { value = 10 };
    A() { f(); }
    static int f(int i=value);
};
