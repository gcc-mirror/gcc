// { dg-do compile { target c++20 } }

template <class T, class U> concept same_as = __is_same_as(T,U);

template<typename T>
concept C1 = requires (T& t) { // { dg-message "in requirements" }
  t.~T(); 
};

template<typename T>
concept C2 = requires (T& t) { // { dg-message "in requirements" }
  { t.~T() } -> same_as<void>;
};

template<typename T>
concept C3 = requires { // { dg-message "in requirements" }
  typename T::type;
};

class S1
{
  ~S1() { }
  using type = int;
};

void driver() {
  static_assert(C1<S1>, ""); // { dg-error "static assertion failed" }
  static_assert(C2<S1>, ""); // { dg-error "static assertion failed" }
  static_assert(C3<S1>, ""); // { dg-error "static assertion failed" }
}

template<typename T>
  requires C1<T>
void f1() { }

template<typename T>
  requires C2<T>
void f2() { }

template<typename T>
  requires C3<T>
void f3() { }

void driver2() {
  f1<S1>(); // { dg-error "unsatisfied|is private" }
  f2<S1>(); // { dg-error "unsatisfied|is private" }
  f3<S1>(); // { dg-error "unsatisfied|is private" }
}
