// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
// GROUPS passed visibility
// visibility file
// From: gfm@mencon.mencon.oz.au (Graham Menhennitt)
// Date:     Wed, 17 Nov 93 21:30:32 EST
// Subject:  gcc 2.5.3 - can't privately inherit and contain same class
// Message-ID: <9311171030.AA00604@mencon>
#include        <iostream>

class A {
public:
        A(void);
};

class B : private A {
public:
        B(void) : A() {}
};

class C : public B {
public:
        C(void) : B(), a() {}

private:
        ::A a;
};
