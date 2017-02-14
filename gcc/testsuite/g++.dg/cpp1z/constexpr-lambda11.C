// Testcase from P0170R1
// { dg-options -std=c++1z }

// 'v' & 'm' are odr-used but do not occur in a constant-expression within the nested
// lambda, so are well-formed.
auto monad = [](auto v) { return [=] { return v; }; };
auto bind = [](auto m) {
  return [=](auto fvm) { return fvm(m()); };
};
// OK to have captures to automatic objects created during constant expression evaluation.
static_assert(bind(monad(2))(monad)() == monad(2)());
