// { dg-do compile } 
// { dg-options "-Wabi" }

struct A { 
  virtual void f(); 
  int f1 : 1; 
};

struct B : public A {
  int f2 : 1;  // { dg-warning "ABI" }
  int : 0;
  int f3 : 4; 
  int f4 : 3;
};
