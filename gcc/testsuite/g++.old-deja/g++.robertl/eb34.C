// Build don't link:
class Base {
public:
  class Bar { public: virtual ~Bar() {}; };
};

class Derived : public Base {
public:
  class Bar : public Base::Bar {};
};

template <class T>
class XYZ : public T::Bar {
};

void test() {
  XYZ<Base> b;
  XYZ<Derived> d;
}
