//PR c++/19439

template<int> struct A
{
  ~A() {}	// { dg-error "with" }
  ~A() {}	// { dg-error "cannot be overloaded" }
};
