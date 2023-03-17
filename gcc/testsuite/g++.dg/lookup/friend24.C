// PR c++/69410

void a();
void f() {
    class A {
        friend void ::a();
        friend class Z;
    };
}
