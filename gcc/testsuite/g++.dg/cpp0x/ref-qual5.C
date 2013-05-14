// 13.1: ...cannot be overloaded if any of them, but not all, have a
// ref-qualifier.

// { dg-require-effective-target c++11 }

class Y {
  void h() &;
  void h() const &;	       // OK
  void h() &&;		       // OK, all declarations have a ref-qualifier
  void i() &;		       // { dg-message "" }
  void i() const;	       // { dg-error "" } prior declaration of i
			       // has a ref-qualifier
};
