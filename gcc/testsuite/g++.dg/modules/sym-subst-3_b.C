// { dg-additional-options "-fmodules-ts -std=c++17" }
export module t;
// { dg-module-cmi t }
import t.s;
struct t;
export void f (t*, decltype (S)){}
// { dg-final { scan-assembler {_Z1fPW1tE1tPW_01sE1s:} } }
