// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M }

export module M;

int x = 123;
void f() {}

int& xr = x;
auto& fr = f;

constexpr int& cxr = xr;
constexpr auto& cfr = fr;
