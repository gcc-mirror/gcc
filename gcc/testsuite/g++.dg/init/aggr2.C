// PR c++/15172
// { dg-do run }

extern "C" int printf (const char *, ...);
extern "C" void abort ();

struct A {
  static A* p;

  A() { p = this; }
  A(const A&);
  ~A() { if (this != p) abort (); }
  void print () { }
};

A* A::p;

struct B {
  A a;
};

B b = { A () };

struct A2 {
  static A2* p;

  A2() { p = this; }
  A2(const A2&);
  ~A2() { if (this != p) abort (); }
  void print () { }
};

A2* A2::p;

struct B2 {
  A2 a2;
};

int main () {
  b.a.print ();
  {
    B2 b2 = { A2() };
    b2.a2.print ();
  }
} 
    
