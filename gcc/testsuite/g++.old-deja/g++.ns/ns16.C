// { dg-do assemble  }

class Y {
public:
  void operator +(int) const;
};

namespace X {
  extern Y const& z;
}

void f(void) {
  X::z + 1;
}
