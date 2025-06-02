// PR c++/113563
// { dg-do run { target c++23 } }

template <typename T>
T&& move(T& value) { return static_cast<T&&>(value); }

template <typename T, typename U> constexpr bool is_same = false;
template <typename T> constexpr bool is_same<T, T> = true;

template <typename T> constexpr bool is_const_ref = false;
template <typename T> constexpr bool is_const_ref<const T> = true;
template <typename T> constexpr bool is_const_ref<const T&> = true;
template <typename T> constexpr bool is_const_ref<const T&&> = true;

template <typename T> constexpr int refqual = 0;
template <typename T> constexpr int refqual<T&> = 1;
template <typename T> constexpr int refqual<T&&> = 2;

struct S {
  int x;

  // 'this' properly acts as a pointer
  auto byref_a() {
    return [this](this auto) { return this->x; };
  }
  auto byref_b() {
    return [this](this auto) { return x; };
  }
  auto byref_c() {
    return [&](this auto) { return x; };
  }
  auto byref_d() {
    return [=](this auto) { return x; };  // { dg-warning "implicit capture" }
  }
  auto byref_e() {
    return [this](this auto) {
      return [this](this auto) {
	return this->x;
      }();
    };
  }
  auto byref_f() {
    return [&](this auto) {
      return [&](this auto) {
	return x;
      }();
    };
  }

  // capturing '*this' stores a copy
  auto byval_a() {
    return [*this](this auto) { return this->x; };
  }
  auto byval_b() {
    return [*this](this auto) { return x; };
  }
  auto byval_c() {
    return [*this](this auto) {
      return [=](this auto) {	// { dg-warning "implicit capture" }
	return this->x;
      }();
    };
  }
  auto byval_d() {
    return [*this](this auto) {
      return [&](this auto) {
	return x;
      }();
    };
  }

  // value category doesn't change with self when capture ref
  auto byref_cat_1() {
    return [this](this auto&& self) {
      static_assert(is_same<decltype((x)), int&>);
    };
  }
  auto byref_cat_2() const {
    return [this](this auto&& self) {
      static_assert(is_same<decltype((x)), const int&>);
    };
  }

  // value category does change with self when capture val
  auto byval_cat() {
    return [*this](this auto&& self) {
      static_assert(is_const_ref<decltype((x))> == is_const_ref<decltype((self))>);
      static_assert(refqual<decltype((x))> == refqual<decltype((self))>);
    };
  }
};

int main() {
  S s{ 5 };

  auto ra = s.byref_a();
  auto rb = s.byref_b();
  auto rc = s.byref_c();
  auto rd = s.byref_d();
  auto re = s.byref_e();
  auto rf = s.byref_f();

  auto va = s.byval_a();
  auto vb = s.byval_b();
  auto vc = s.byval_c();
  auto vd = s.byval_d();

  s.x = 10;

  if (ra() != 10
      || rb() != 10
      || rc() != 10
      || rd() != 10
      || re() != 10
      || rf() != 10)
    __builtin_abort();

  if (va() != 5
      || vb() != 5
      || vc() != 5
      || vd() != 5)
    __builtin_abort();

  auto r_nonconst_1 = s.byref_cat_1();
  const auto r_const_1 = r_nonconst_1;
  r_nonconst_1();
  move(r_nonconst_1)();
  r_const_1();
  move(r_nonconst_1)();

  auto r_nonconst_2 = s.byref_cat_2();
  const auto r_const_2 = r_nonconst_2;
  r_nonconst_2();
  move(r_nonconst_2)();
  r_const_2();
  move(r_nonconst_2)();

  auto v_nonconst = s.byval_cat();
  const auto v_const = v_nonconst;
  v_nonconst();
  move(v_nonconst)();
  v_const();
  move(v_nonconst)();
}
