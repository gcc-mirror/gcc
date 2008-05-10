// PR rtl-optimization/36185
// { dg-do run }
// { dg-options "-O2 -fgcse-sm" }

struct Base {
        virtual ~Base() {}
        virtual void f() = 0;
};
struct Derived : Base {
        Derived();
        virtual void f() {}
};
struct Foo {
        Foo(Base&);
};
Derived::Derived() {
        Foo foo(*this);
}
Foo::Foo(Base& base) {
        base.f();
}
int main() {
        Derived d;
}
