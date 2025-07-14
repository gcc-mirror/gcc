// { dg-do compile { target c++26 } }
// Test consteval blocks, as specified by P2996.

consteval {
  template <class T> // { dg-error "template declaration cannot appear at block scope" }
  struct X { };

  template <class T> // { dg-error "template declaration cannot appear at block scope" }
  concept C = true;

  return; // OK
}
