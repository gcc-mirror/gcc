struct A {
    int *i;
    A();
    ~A();
};

static int x = 0;

A::A() : i(&x) {}
A::~A() {}

