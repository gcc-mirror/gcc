// { dg-do compile { target c++2a } }

struct A {
  int &r;			// { dg-message "reference" }
  bool operator==(const A&) const = default; // { dg-message "deleted" }
};

int i;
A a { i };
bool b = a == a;		// { dg-error "deleted" }
