// PR c++/14668

class A {}; // { dg-error "" }
class B { 
  static A *A; // { dg-error "" }
}; 
A *B::A = 0;
