// Testcase from P0170R1
// { dg-options -std=c++1z }

static_assert([](int n) { return [&n] { return ++n; }(); }(3) == 4);
