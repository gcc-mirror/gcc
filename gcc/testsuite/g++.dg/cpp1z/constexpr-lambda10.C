// Testcase from P0170R1
// { dg-do compile { target c++17 } }

void g() {
  const int n = 0;
  [=] {
    constexpr int i = n; // OK, 'n' is not odr-used and not captured here.
    constexpr int j = *&n; // { dg-error "" } '&n' would be an odr-use of 'n'.
  };
}
