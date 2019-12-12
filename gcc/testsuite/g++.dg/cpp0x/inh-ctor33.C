// PR c++/77747
// { dg-do compile { target c++11 } }

class X {
public:
  X() { }
  X(int a) { }
};

class Y : public X { };

class Z : public Y {
  using X::X; // { dg-error "cannot inherit constructors from indirect base .X." }
};

int main()
{
  Z z{3}; // { dg-error "no matching" }
}
