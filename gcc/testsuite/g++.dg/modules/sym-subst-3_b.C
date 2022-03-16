// { dg-additional-options "-fmodules-ts -std=c++17" }
export module t;
// { dg-module-cmi t }
import t.s;
struct t;
export void f (t*, decltype (S)){}
// { dg-final { scan-assembler {_ZW1t1fPS_1tPS_W1s1s:} } }
