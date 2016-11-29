// { dg-do compile }
// { dg-options "" }

template <class x>
class z : x
{
public:
  bool zz () { return false; }
  int f () { return zz ? : 1; } // { dg-error "cannot convert" }
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
