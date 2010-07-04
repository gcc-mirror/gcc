// PR c++/44486 missing space in __PRETTY_FUNCTION__ expansion in anonymous namespace
// { dg-do compile }
// { dg-options "" }

struct S { };
namespace { S f() { const char * s = __PRETTY_FUNCTION__; return S(); } }

int main() { f(); }

// { dg-final { scan-assembler "S \{anonymous\}::f" } }
