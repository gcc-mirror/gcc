// PR c++/70077
// { dg-do compile { target c++11 } }

struct B {
    B(int) noexcept { }
    virtual void f() = 0;
};

struct D: public B {
    using B::B;
    D() noexcept(noexcept(D{42})): B{42} { }
    void f() override { }
};

int main() {
    B *b = new D{};
}
