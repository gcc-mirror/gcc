// Build don't link: 
// GROUPS passed old-abort
struct A {
    void a1();
    void a2();
};

struct B {
    void A::a1(); // this used to die in chainon(), now grokdeclarator should// ERROR -  cannot declare.*
    void A::a2(); // should be fixed by the 930629 change.// ERROR -  cannot declare.*
};
