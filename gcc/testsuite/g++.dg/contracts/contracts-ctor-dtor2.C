// Tests to ensure that an invalid this parm cannot be used in pre on ctors or
// in post on dtors.
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

struct F0
{
  F0() [[ pre: x > 0 ]]; // { dg-error "invalid use of member" }
  ~F0() [[ post: x > 0 ]]; // { dg-error "invalid use of member" }
  int x{-1};
};

struct F1
{
  F1() [[ pre: this->x > 0 ]]; // { dg-error "may not be used" }
  ~F1() [[ post: this->x > 0 ]]; // { dg-error "may not be used" }
  int x{-1};
};

struct F2
{
  F2()
    [[ post ret: false ]] // { dg-error "does not return a value" }
  {
  }
  ~F2()
    [[ post r: false ]] // { dg-error "does not return a value" }
  {
  }
  void f()
    [[ post r: false ]] // { dg-error "does not return a value" }
  {
  }
};

