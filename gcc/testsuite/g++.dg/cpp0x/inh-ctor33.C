// PR c++/77747
// { dg-do compile { target c++11 } }

class X {
public:
  X() { }
  X(int a) { }
};

class Y : public X { };

class Z : public Y {
  using X::X; // { dg-error ".X. is not a direct base of .Z." }
};

int main()
{
  Z z{3}; // { dg-error "no matching" }
}
