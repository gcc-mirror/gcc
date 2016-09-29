// PR c++/23839

class C
{
  int i;
public:
  C(int j) : i(j) { }
  operator int() { return i; }
};

C f (register C x)	// { dg-error "ISO C\\+\\+1z does not allow 'register' storage class specifier" "" { target c++1z } }
{
  return x + 31;
}
