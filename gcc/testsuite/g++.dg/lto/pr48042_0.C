// { dg-lto-do link }
// { dg-extra-ld-options "-r -nostdlib -g -flinker-output=nolto-rel" }

class A {
    virtual int x() = 0;
};

class B:public A {
    int x();
};

int B::x() {
    return 0;
}
