// Build don't link: 
// GROUPS passed conversions
// cvt file
// Message-Id: <CC7oHn.B4F@izf.tno.nl>
// From: tom@izfcs.izf.tno.nl (Tom Vijlbrief)
// Subject: g++ 2.4.5 has problems with NON virtual shared base classes
// Date: Mon, 23 Aug 1993 12:10:34 GMT


#include        <stdio.h>

#define FAIL

class Base {
public:
  Base() { printf("Base::Base\n"); }
  virtual ~Base() { printf("Base::~Base\n"); }
  virtual void v() { printf("Base::v\n"); }
};

class Base2 {
public:
  Base2() { printf("Base2::Base2\n"); }
  virtual ~Base2() { printf("Base2::~Base2\n"); }
  virtual void v() { printf("Base2::v\n"); }
};

class A: public Base {
public:
  A() { printf("A::A\n"); }
  ~A() { printf("A::~A\n"); }
  virtual void va() { printf("A::va\n"); }
};

#ifdef FAIL
class B: public Base {
#else
class B: public Base2 {
#endif
public:
  B() { printf("B::B\n"); }
  ~B() { printf("B::~B\n"); }
  virtual void vb() { printf("B::vb\n"); }
};

class C: public A, public B {
public:
  C() { printf("C::C\n"); }
  ~C() { printf("C::~C\n"); }
  void va() { printf("C::va\n"); }
};


int main()
{
  C *cp= new C;
  cp->va();
  delete cp;
}

