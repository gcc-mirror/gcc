// { dg-do assemble  }
// GROUPS passed visibility
// visibility file
// From: klamer@mi.el.utwente.nl (Klamer Schutte)
// Date:     Thu, 12 Aug 93 12:03:09 +0200
// Subject:  g++ 2.4.5 failed to report a bug
// Message-ID: <9308121003.AA02294@mi.el.utwente.nl>
class A {
protected:
      void foo(); // { dg-error "" } protected
};

class B : public A
{
        void bar(A &a)
                {       a.foo(); }// { dg-error "" } .*
};
