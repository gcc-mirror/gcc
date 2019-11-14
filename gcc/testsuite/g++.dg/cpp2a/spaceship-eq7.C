// { dg-do compile { target c++2a } }

union A
{
  int i;
  bool operator==(const A&) const = default; // { dg-message "union" }
};

A a { 42 };
bool b = a == a;		// { dg-error "deleted" }
