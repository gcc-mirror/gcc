// PR c++/62207

template<class F>
class X {
public:
  template<F f> class Y {};
  template<F f> void y() {}
  X(F f)
  {
    Y<f> y;  // { dg-error "not a constant" }

    y.value();
  }
};

int main() { X<int> x(1); }
