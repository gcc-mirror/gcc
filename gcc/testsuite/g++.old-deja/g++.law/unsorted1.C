// { dg-do assemble  }
// { dg-options "-O" }
// GROUPS passed unsorted
// unsorted.2 file
// From: skipnyc!skipsun!skip@fsg.com (Skip Gilbrech)
// Date:     Wed, 10 Jun 92 6:55:18 EDT
// Subject:  Problem with derived class access adjustment and -O
// Message-ID: <9206101055.AA20593@skipsun.UUCP>


class A {
  public:
    virtual void func() = 0;
};

class B : public A {
  public:
    void func() {}
};

class C : private B {
  public:
    B::func;
};

class D {
    C c;
  public:
    void func() { c.func(); }
};

