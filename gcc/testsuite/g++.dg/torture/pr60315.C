// PR ipa/60315
// { dg-do compile }
// { dg-options "-std=c++11" }

struct Base {
    virtual int f() = 0;
};

struct Derived : public Base {
    virtual int f() final override {
        return 42;
    }
};

extern Base* b;

int main() {
    return (static_cast<Derived*>(b)->*(&Derived::f))();
}
