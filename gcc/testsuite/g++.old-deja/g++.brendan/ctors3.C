// { dg-do assemble  }
// GROUPS passed constructors
class A;

class B {
public:
    B();
static A sa;
};

class A {
public:
    A(int i);
};

A B::sa(1);

