// ensure that passing asserts do not affect constexpr functions
// ensure that failing asserts generate an error in a constexpr function
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

constexpr int wfun(int a) {
  [[assert: a > 0]];
  return a;
}

constexpr int ffun(int a) {
  [[assert: a > 0]];
  return a;
}

template<typename T>
constexpr int tfun(T a) {
  [[assert: a > 0]];
  return a;
}

template<typename T>
constexpr int wtfun(T a) {
  [[assert: a > 0]];
  return a;
}

template<typename T>
constexpr int ftfun(T a) {
  [[assert: a > 0]];
  return a;
}

constexpr int explicitfn(int a) {
  [[assert ignore: a > 0]];
  [[assert check_never_continue: a > 0]];
  return a;
}

int main(int, char **) {
  constexpr int a = wfun(10);
  constexpr int b = ffun(-10); // { dg-message "in .constexpr. expansion" }
  // { dg-error "contract predicate" "" { target *-*-* } 12 }
  constexpr int c = wtfun(10);
  constexpr int d = ftfun(-10);  // { dg-message "in .constexpr. expansion" }
  // { dg-error "contract predicate" "" { target *-*-* } 30 }
  constexpr int e = explicitfn(-10); // { dg-message "in .constexpr. expansion" }
  // { dg-error "contract predicate" "" { target *-*-* } 36 }
  return 0;
}

