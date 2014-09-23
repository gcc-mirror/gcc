// PR c++/61857
// { dg-do compile { target c++14 } }

struct A { 
  A(int val) {}
};

A a{ [x=123]{ return x; }() }; 
