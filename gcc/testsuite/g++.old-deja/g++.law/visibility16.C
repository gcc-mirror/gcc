// Build don't link: 
// GROUPS passed visibility
// visibility file
// From: Marie Trapp <Marie.Trapp@analog.com>
// Date:     Thu, 5 Aug 93 11:55:15 EDT
// Subject:  access of protected members
// Message-ID: <9308051553.AA07639@nwd2sun1.analog.com>
class A {
  protected:
    int astuff; // ERROR - protected
    A() {
        astuff = 3; 
    }
};

class B : public A {
    int bstuff;
  public:
    B( A *p) {
        bstuff = p->astuff;// ERROR - .*
    }
};

class C : public A {
    int cstuff;
  public:
    C() {
        cstuff = 5;
    }
};

int main() {
    C cvar;
    B bvar(&cvar);
}
