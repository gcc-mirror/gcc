// { dg-do assemble  }
// GROUPS passed miscellaneous-bugs

struct A {
        int     aa;
};
struct B : public A {
};
struct C : public A {
};
struct D : public C, public B {
        void fun() { C::aa = 10; }
};
