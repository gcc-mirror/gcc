// Build don't link: 
// GROUPS passed visibility
// visibility file
// rom: roland@jts.com (Roland Knight )
// Date:     Sat, 8 May 1993 17:27:35 -0400
// Subject:  gcc 2.3.3 protected member access bug
// Message-ID: <9305082127.AA19577@icepick.jts.com>

class A {
protected:
    int a; // ERROR - protected
};

class B : public A {
public:
    void f1(A* pa);
};


void B::f1(A* pa) {
    pa->a = 1;    // illegal but allowed by gcc// ERROR - .*
}
