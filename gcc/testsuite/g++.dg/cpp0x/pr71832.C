// { dg-do compile { target c++11 } }

template < typename decltype (0) > struct A  // { dg-error "expected|two or more" }
{ 
  void foo () { baz (); }
  template < typename ... S > void baz () {}
};
