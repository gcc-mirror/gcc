// { dg-do assemble  }

class Base {
public:
  class Bar { public: virtual ~Bar() {} };
};

class Derived : public Base {
public:
  class Bar : public Base::Bar {};
};

template <class T>
struct XYZ : public T::Bar {
  XYZ(): T::Bar() { }
};

void test() {
  XYZ<Base> b;
  XYZ<Derived> d;
}
