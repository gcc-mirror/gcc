// { dg-do assemble  }
// GROUPS passed visibility
// visibility file
// From: Rob Hasker <hasker@sparc0a.cs.uiuc.edu>
// Date:     Sat, 3 Apr 1993 13:19:05 -0600
// Subject:  no privacy
// Message-ID: <199304031919.AA20554@sparc17.cs.uiuc.edu
class Top {
public:
    Top() {}
      void val() {} // { dg-message "" } private base class
};

class Derived : private Top {
public:
    Derived() {}
};

class Unrelated {
    Derived derived;
public:
    void oops() { derived.val(); }// { dg-error "" } .*
};
