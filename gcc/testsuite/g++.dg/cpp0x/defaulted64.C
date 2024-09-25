// PR c++/116162
// { dg-do compile { target c++11 } }

struct M
{
  M& operator=(M&&);
};

struct R
{
  R& operator=(R&&) = default;
  M m;
};

struct S
{
  S& operator=(const S&&) = default; // { dg-warning "implicitly deleted" "" { target c++20 } }
				     // { dg-error "does not match" "" { target c++17_down } .-1 }

  M m;
};

struct T
{
  T operator=(T&&) = default; // { dg-error "defaulted" }
  M m;
};
