// PR c++/14668

class A {}; // { dg-message "declared here" }
class B { 
  static A *A; // { dg-error "changes meaning" }
}; 
A *B::A = 0;
