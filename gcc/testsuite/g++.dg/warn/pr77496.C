// { dg-do compile }
// { dg-options "-Wparentheses" }

template <class x>
class z : x
{
public:
  bool zz () { return false; }
  int f () { return zz () ? : 1; } // { dg-warning "omitted middle operand" }
};

class t
{
};

int
main ()
{
  z<t> x;
  return x.f ();
}
