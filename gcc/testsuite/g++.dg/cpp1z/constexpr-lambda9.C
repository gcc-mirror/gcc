// Testcase from P0170R1
// { dg-do compile { target c++17 } }

static_assert([](int n) { return [&n] { return ++n; }(); }(3) == 4);
