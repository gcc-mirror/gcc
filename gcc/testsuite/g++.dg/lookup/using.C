// { dg-do compile }

struct X { };
struct Y { };
struct Z { };

struct Base {
  X f() { return X(); }
  Y f() const { return Y(); }
};

struct Derived : Base {
  using Base::f;
  Z f(int) { return Z(); }
};

int main()
{
  Derived d;
  X x = d.f();                  // { dg-bogus "Y" "" }
}


