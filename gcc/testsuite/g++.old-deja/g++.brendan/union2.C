// Build don't link: 
// GROUPS passed unions
class B;
 
struct A {
    A(B* x) : i(x) {}
    A() : i(0) {}
 
    union {
        B* i;
        B* c;
    };
};
