// { dg-do assemble  }
// PRMS Id: 5085
// Bug: TYPE_POINTER_TO wasn't set.

class A {
   A(const A &);
   void operator=(const A &); 
public:
   inline A();
};

class B {
   A a;
public:
   B();
   virtual void f() const;
};

class C : public B { };

class D : C {
public:
  void f() const;
};

void D::f() const
{
  C::f();
}
