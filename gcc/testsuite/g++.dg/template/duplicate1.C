//PR c++/19439

template<int> struct A
{
  ~A() {}	// { dg-message "previous" }
  ~A() {}	// { dg-error "cannot be overloaded" }
};
