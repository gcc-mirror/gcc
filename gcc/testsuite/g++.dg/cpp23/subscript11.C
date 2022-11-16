// { dg-do compile { target c++11 } }
// { dg-options "" }

struct S {
  static int &operator[] (int);	// { dg-warning "may be a static member function only with" "" { target c++20_down } }
  static int &operator[] ();	// { dg-warning "may be a static member function only with" "" { target c++20_down } }
				// { dg-error "'static int& S::operator\\\[\\\]\\\(\\\)' must have exactly one argument" "" { target c++20_down } .-1 }
  static int &operator[] (int, int, int);	// { dg-warning "may be a static member function only with" "" { target c++20_down } }
				// { dg-error "'static int& S::operator\\\[\\\]\\\(int, int, int\\\)' must have exactly one argument" "" { target c++20_down } .-1 }
  int s;
};
