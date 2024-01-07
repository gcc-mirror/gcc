// P0847R7
// { dg-do compile { target c++23 } }

// basic use cases and calling

// non-trailing return
// definitions
struct S0 {
  void f0(this S0) {}
  void f1(this S0&) {}
  void f2(this S0&&) {}
  void f3(this S0 const&) {}
  void f4(this S0 const&&) {}
  template<typename Self>
  void d0(this Self&&) {}
  void d1(this auto&&) {}
};
// declarations
struct S1 {
  void f0(this S1);
  void f1(this S1&);
  void f2(this S1&&);
  void f3(this S1 const&);
  void f4(this S1 const&&);
  template<typename Self>
  void d0(this Self&&);
  void d1(this auto&&);
};
// out of line definitions
void S1::f0(this S1) {}
void S1::f1(this S1&) {}
void S1::f2(this S1&&) {}
void S1::f3(this S1 const&) {}
void S1::f4(this S1 const&&) {}
template<typename Self>
void S1::d0(this Self&&) {}
void S1::d1(this auto&&) {}

// trailing return
// definitions
struct S2 {
  auto f0(this S2) -> void {}
  auto f1(this S2&) -> void {}
  auto f2(this S2&&) -> void {}
  auto f3(this S2 const&) -> void {}
  auto f4(this S2 const&&) -> void {}
  template<typename Self>
  auto d0(this Self&&) -> void {}

  auto d1(this auto&&) -> void {}
};
// declarations
struct S3 {
  auto f0(this S3) -> void;
  auto f1(this S3&) -> void;
  auto f2(this S3&&) -> void;
  auto f3(this S3 const&) -> void;
  auto f4(this S3 const&&) -> void;
  template<typename Self>
  auto d0(this Self&&) -> void;
  auto d1(this auto&&) -> void;
};
// out of line definitions
auto S3::f0(this S3) -> void {}
auto S3::f1(this S3&) -> void {}
auto S3::f2(this S3&&) -> void {}
auto S3::f3(this S3 const&) -> void {}
auto S3::f4(this S3 const&&) -> void {}
template<typename Self>
auto S3::d0(this Self&&) -> void {}
auto S3::d1(this auto&&) -> void {}

template<typename T>
void call_with_qualification()
{
  T obj{};
  // by value should take any qualification (f0)
  T{}.f0();
  obj.f0();
  static_cast<T&&>(obj).f0(); 
  static_cast<T const&>(obj).f0();
  static_cast<T const&&>(obj).f0();
  // specific qualification (f1 - f4)
  T{}.f2();
  T{}.f3();
  T{}.f4();
  obj.f1();
  obj.f3();
  static_cast<T&&>(obj).f2();
  static_cast<T&&>(obj).f3();
  static_cast<T&&>(obj).f4();
  static_cast<T const&>(obj).f3();
  static_cast<T const&&>(obj).f4();
  // deduced should (obviously) take any qualification (d0, d1)
  T{}.d0();
  obj.d0();
  static_cast<T&&>(obj).d0();
  static_cast<T const&>(obj).d0();
  static_cast<T const&&>(obj).d0();
  T{}.d1();
  obj.d1();
  static_cast<T&&>(obj).d1();
  static_cast<T const&>(obj).d1();
  static_cast<T const&&>(obj).d1();
}

void perform_calls()
{
  call_with_qualification<S0>();
  call_with_qualification<S1>();
  call_with_qualification<S2>();
  call_with_qualification<S3>();
}

