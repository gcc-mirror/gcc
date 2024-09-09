// PR c++/116162
// { dg-do compile { target c++11 } }

struct S
{
  S& operator=(S &&) = default;
};

struct T
{
  T& operator=(volatile T &&) = default; // { dg-error "defaulted" "" { target c++17_down } }
					 // { dg-warning "implicitly deleted" "" { target c++20 } .-1 }
};

struct U
{
  U& operator=(const volatile U &&) = default; // { dg-error "defaulted" "" { target c++17_down } }
					       // { dg-warning "implicitly deleted" "" { target c++20 } .-1 }
};

struct V
{
  V& operator=(const V &&) = default; // { dg-error "defaulted" "" { target c++17_down } }
				      // { dg-warning "implicitly deleted" "" { target c++20 } .-1 }
};
