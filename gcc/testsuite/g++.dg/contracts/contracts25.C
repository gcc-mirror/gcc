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

// { dg-output "default std::handle_contract_violation called: .*.C 12 ffun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 30 ftfun<int> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 36 explicitfn .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 30 ftfun<double> .*(\n|\r\n|\r)*" }

