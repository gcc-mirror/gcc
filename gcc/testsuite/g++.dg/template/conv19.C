// PR c++/101698
// { dg-do compile { target c++11 } }

class Base {
 public:
  template <class T>
  operator const T&() const = delete;

  virtual operator const int&() const {
    static int res;
    return res;
  }
};

template <class T>
class Derive : public Base {
 public:
  operator const T&() const override {
    using Y = int;
    //static_assert(__is_same_as(T,Y), "");

    static int res;

    res = Base::operator const Y&(); // OK
    res = Base::operator const T&(); // { dg-bogus "deleted" }
    return res;
  }
};

int main() {
  Derive<int> a;
  const int& b = a;
  (void)b;
}
