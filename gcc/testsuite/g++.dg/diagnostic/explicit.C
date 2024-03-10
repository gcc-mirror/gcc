// { dg-do compile { target c++11 } }

struct A {
  long int l;
};

struct S {
  explicit S(int) { }
  explicit operator bool() const { return true; } // { dg-message "explicit conversion function was not considered" }
  explicit operator int() const { return 42; } // { dg-message "explicit conversion function was not considered" }
  explicit operator char() const { return 42; } // { dg-message "explicit conversion function was not considered" }
  explicit operator double() const { return 42.; } // { dg-message "explicit conversion function was not considered" }
  explicit operator float() const { return 42.; } // { dg-message "explicit conversion function was not considered" }
  explicit operator long() const { return 42.; } // { dg-message "explicit conversion function was not considered" }
};

double
f (char)
{
  return S{2}; // { dg-error "cannot convert .S. to .double. in return" }
}

void
g ()
{
  S s = {1}; // { dg-error "would use explicit constructor" }
  bool b = S{1}; // { dg-error "cannot convert .S. to .bool. in initialization" }
  int i;
  i = S{2}; // { dg-error "cannot convert .S. to .int. in assignment" }
  f (S{3}); // { dg-error "cannot convert .S. to .char." }
  A a{ S{4} }; // { dg-error "cannot convert .S. to .long int. in initialization" }
  float arr[1] = { S{5} }; // { dg-error "cannot convert .S. to .float. in initialization" }
}
