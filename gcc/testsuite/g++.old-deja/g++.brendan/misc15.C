// Build don't link: 
// GROUPS passed miscellaneous-bugs
// we shouldn't get any warnings or errors for this code
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
