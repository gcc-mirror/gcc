// PR c++/116162
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-defaulted-function-deleted" }

struct M
{
  M& operator=(M&);
};

struct T
{
  // if F1 is an assignment operator, and the return type of F1 differs
  // from the return type,  the program is ill-formed.
  T operator=(T&) = default; // { dg-error "defaulted" }
  M m;
};

struct U
{
  // if F1's non-object parameter type is not a reference, the program
  // is ill-formed.
  U& operator=(U) = default; // { dg-error "defaulted" }
  M m;
};
