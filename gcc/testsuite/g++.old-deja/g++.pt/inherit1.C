// { dg-do run  }
// Origin: Wolfgang Bangerth <wolf@gaia.iwr.uni-heidelberg.de>

int i = 1;

struct Base1 {  int local1;  };
struct Base2 {  int local2;  };

template <int dim> class Derived;

template <>
class Derived<1> : public Base1, public Base2 {};

template <int dim>
class FinalClass :  public Derived<dim> {
public:
  FinalClass () {
    if (&this->local1 != &this->local2)
      i = 0;
  }
};

int main () {
  FinalClass<1> a1;
  return i;
}
