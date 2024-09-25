// PR c++/116162
// { dg-do compile { target c++23 } }

struct M
{
  M& operator=(M&);
};

struct T
{
  // if F1 is an assignment operator, and the return type of F1 differs
  // from the return type,  the program is ill-formed.
  T operator=(this T&, T&) = default; // { dg-error "defaulted" }
  M m;
};

struct U
{
  // if F1's non-object parameter type is not a reference, the program
  // is ill-formed.
  U& operator=(this U&, U) = default; // { dg-error "defaulted" }
  M m;
};
