// { dg-do assemble  }
// PRMS Id: 4342
// Bug: g++ fails to massage ambiguity in calling virtual destructor.

class A { public: virtual ~A();};
class B: public A { };
class C: public A { };
class D: public B, public C { };
 
void foo ()
{
    D* p = new D;
    delete p;
}
