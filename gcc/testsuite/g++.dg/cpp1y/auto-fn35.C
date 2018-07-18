// PR c++/77790
// { dg-do compile { target c++11 } }

template < typename S > struct A
{
  // { dg-error "" "" { target c++11_only } .+1 }
  template < typename T > static auto f () { return 0; } 
  template < class U = decltype (f < S > ()) > int g () { return 0; }
};

auto a = A < int > {}.g ();
