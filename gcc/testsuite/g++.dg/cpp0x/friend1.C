// From N1791
// { dg-options -std=c++0x }

class C;
typedef C Ct;
class X1 {
  friend C;		// OK: class C is a friend
};

class X2
{
  friend Ct;		// OK: class C is a friend
  friend D;		// { dg-error "" } no type-name D in scope
  friend class D;	// OK: elaborated-type-specifier declares new class
};

template <typename T> class R {
  friend T;
};

R<C> rc;		// class C is a friend of R<C>
R<int> Ri;		// OK: "friend int;" is ignored
