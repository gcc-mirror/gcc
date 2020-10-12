// { dg-do compile { target c++20 } }

template<typename T>
concept check_c = false;

template<typename T>
concept c1 = requires (T x) {
 requires check_c<decltype(x)>;
};

template<c1 T>
void f1() { }

template<typename T>
void f2(T x) requires requires { requires check_c<decltype(x)>; } { }


template<typename T>
constexpr bool check_f() { return false; }

template<typename T>
concept c2 = requires (T x) {
 requires check_f<decltype(x)>();
};

template<c2 T>
void f3() { }

template<typename T>
void f4(T x) requires requires { requires check_f<decltype(x)>(); } { }


template<typename T>
constexpr bool check_v = false;

template<typename T>
concept c3 = requires (T x) {
 requires check_v<decltype(x)>;
};

template<c3 T>
void f5() { }

template<typename T>
void f6(T x) requires requires { requires check_v<decltype(x)>; } { }


void test()
{
  f1<int>(); // { dg-error "no match" }
  f2(0); // { dg-error "no match" }

  f3<int>(); // { dg-error "no match" }
  f4(0); // { dg-error "no match" }

  f5<int>(); // { dg-error "no match" }
  f6(0); // { dg-error "no match" }
}
