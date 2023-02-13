// ensure that passing asserts do not affect constexpr functions
// ensure that failing asserts generate an error at runtime in constexpr funcs
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }

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
  [[assert check_maybe_continue: a > 0]];
  return a;
}

int main(int, char **) {
  int a = wfun(10);
  int b = ffun(-10);
  int c = wtfun(10);
  int d = ftfun(-10);

  int e = explicitfn(-10);

  int z = ftfun(-10.0);

  return 0;
}

// { dg-output "contract violation in function ffun at .*.C:12: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function ftfun<int> at .*.C:30: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function explicitfn at .*.C:36: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function ftfun<double> at .*.C:30: .*(\n|\r\n|\r)" }

