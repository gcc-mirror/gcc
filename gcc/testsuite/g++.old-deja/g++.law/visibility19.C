// { dg-do assemble  }
// GROUPS passed visibility
// visibility file
// From: mclaugh@tnt.acsys.com (Mark A. McLaughlin)
// Date:     Wed, 25 Aug 93 14:30:47 MDT
// Subject:  g++ bug
// Message-ID: <9308252030.AA02352@tnt.acsys.com>
class B {
protected:
    int i; // { dg-error "" } protected
};

class D1 : public B {
};

class D2 : public B {
    friend void fr(B*,D1*,D2*);
    void mem(B*,D1*);
};

void fr(B* pb, D1* p1, D2* p2)
{
    pb->i = 1;  // illegal// { dg-error "" } .*
    p1->i = 2;  // illegal// { dg-error "" } .*
    p2->i = 3;  // ok (access through D2)
}

void D2::mem(B* pb, D1* p1)
{
    pb->i = 1;  // illegal// { dg-error "" } .*
    p1->i = 2;  // illegal// { dg-error "" } .*
    i = 3;      // ok (access through `this')
}

void g(B* pb, D1* p1, D2* p2)
{
    pb->i = 1;  // illegal// { dg-error "" } .*
    p1->i = 2;  // illegal// { dg-error "" } .*
    p2->i = 3;  // illegal// { dg-error "" } .*
}
