// { dg-do compile { target c++2a } }

struct A
{
  union { int i; }			     // { dg-message "union" }
  bool operator==(const A&) const = default; // { dg-message "deleted" }
};

A a { 42 };
bool b = a == a;		// { dg-error "deleted" }
