// PR c++/89331

class A {
public:
    char a;
};

class B : public A {
public:
    static const unsigned b = __builtin_offsetof(B, a); // { dg-error "incomplete" }
};
