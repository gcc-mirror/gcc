// { dg-do assemble  }
// GROUPS passed visibility
// visibility file
// From: doug@foxtrot.ccmrc.ucsb.edu (Douglas Scott)
// Date:     Tue, 10 Aug 93 10:06:33 PDT
// Subject:  G++ 2.4.5 allows access to protected base members
// Message-ID: <9308101706.AA04485@foxtrot.ccmrc.ucsb.edu>

class Base {
protected:
      void protectedBaseFunction() {} // { dg-message "" } protected
public:
    Base() {}
};


class Derived : public Base {
public:
    Derived() {}
    void noticeThisFunction(Base *);
};


void
Derived::noticeThisFunction(Base *b) {
    b->protectedBaseFunction(); // ARM says this is not allowed// { dg-error "" } .*
                                // since it is not called on 'this'
}

int main() {
    Base b;
    Derived d;
    d.noticeThisFunction(&b);
    printf("gpptest run\n");// { dg-error "5:'printf' was not declared" } .*
}

