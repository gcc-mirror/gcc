// PR c++/61971
// { dg-options "-O2 -Warray-bounds" }

class B {
public:
    virtual ~B(){};
};

class A {
public:
    B   m1[1];
    B   m2[1];
    B   m3[1];

    A(){};
};

int main() {
        A v;
        return 0;
}
