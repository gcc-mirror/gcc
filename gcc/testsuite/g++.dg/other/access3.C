// { dg-do compile }
// { dg-options "-fno-access-control" }

// PR c++/20022

// Make sure -fno-access-control doesn't crash, and actually grants at
// least some access.

class B {
  enum A {};
};

B::A r;
