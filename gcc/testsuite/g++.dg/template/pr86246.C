// { dg-do compile { target c++11 } }
// PR c++/86246 ICE in tsubst

namespace std {
  template<typename T> struct is_class {
    static constexpr bool value = true;
  };
  template<> struct is_class<double> {
    static constexpr bool value = false;
  };
}

class MyClass {
 public:
  operator double() const {
    return 1;
  }
  template<typename T>
  operator T() const {
    static_assert(std::is_class<T>::value, "problem");
    return T();
  }
};

template<typename T>
void SetValue(const MyClass& obj, T* value) {
  //  erroneously dispatched to operator T when T is double
  *value = obj.operator T();
}

int main() {
  MyClass obj;
  // works fine
  obj.operator double ();
  double x;
  // error, when operator T is called in SetValue
  SetValue(obj, &x);
}
