// PR c++/62155
// { dg-do compile { target c++11 } } 

template <typename T> struct S {
  T i{[this] {}};		// { dg-error "cannot convert" }
};

S<int> s;			// { dg-message "required" }
