struct Base1 {
    virtual ~Base1() {}
};
struct Base2 {
    virtual void f() = 0;
};
struct Base : Base1, Base2 {
    virtual void f();
};
