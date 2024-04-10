// PR c++/113332
// { dg-do compile { target c++11 } }

struct tuple {
  template<class _Tp>
  static constexpr bool __is_implicitly_default_constructible() { return true; }

  template<class _Tp = void,
           bool = __is_implicitly_default_constructible<_Tp>()>
  tuple();
};

struct DBusStruct {
private:
  tuple data_;
};

struct IBusService {
  int m = [] { DBusStruct{}; return 42; }();
};
