// PR c++/6402
// Origin: Wolfgang Bangerth <wolfgang@dealii.org>
// { dg-do compile }

class A
{
    A (const A&);
  public:
    A();
};

struct B { A a; };

struct C : public B
{
    void foo() { const A &ref = B::a; } // taking reference, not a copy!
};
