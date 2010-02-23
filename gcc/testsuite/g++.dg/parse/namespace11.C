// PR c++/43069

namespace std {
  template < typename >
  void swap ();
}
template std::swap		// { dg-error "" }
