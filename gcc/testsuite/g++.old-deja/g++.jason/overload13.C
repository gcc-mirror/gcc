// { dg-do run  }
// Bug: g++ screws up derived->base conversions when calling a global function
// in the presence of matching members in the base.  Whew.

struct xios {
  virtual ~xios() { }
};

struct xistream: virtual public xios {
  int j;
  void operator>>(char&);
};

struct xfstreambase: virtual public xios { };

struct xifstream: public xfstreambase, public xistream { };

void operator>>(xistream& i, int j)
{
  i.j = 0;
}

int main() {
  int i;
  xifstream ifs;
  
  ifs >> i;
}
