// { dg-do assemble  }
// GROUPS passed constructors
// ctor file
// From: mln@tera.com (Mark Niehaus)
// Subject: g++-2.5.2 -- private copy ctor hides public ctor
// Date: Mon, 8 Nov 93 10:14:50 PST

class A {
    A(A&);
  public:
    A();
};

class B {
    A a;
};
