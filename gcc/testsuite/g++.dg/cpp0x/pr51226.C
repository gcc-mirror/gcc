// PR c++/51226
// { dg-do compile { target c++11 } }

template<int> struct A           // { dg-message "provided" }
{
  enum E : int;
};

template<> enum A<>::E : int {}  // { dg-error "wrong number" }

// { dg-prune-output "expected" }
// { dg-prune-output "specialization" }
