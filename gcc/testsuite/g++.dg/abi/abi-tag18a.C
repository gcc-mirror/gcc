// { dg-skip-if "PR 70349" { hppa*-*-hpux* } }
// { dg-options "-fabi-version=9 -fno-implicit-constexpr" }
// { dg-final { scan-assembler "_Z1fB7__test1v" } }
// { dg-final { scan-assembler "_ZZ1fB7__test1vEN1T1gB7__test2Ev" } }
// { dg-final { scan-assembler "_ZZZ1fB7__test1vEN1T1gEvE1x" } }
// { dg-final { scan-assembler "_ZGVZZ1fvEN1T1gEvE1x" } }

struct X { ~X(); };
inline namespace __test1 __attribute__((abi_tag)) { struct A1 { }; }
inline namespace __test2 __attribute__((abi_tag)) { struct A2 { }; }
inline A1 f() {
  struct T {
    A2 g() {
      static X x;
      return A2();
    }
  };
  T().g();
  return A1();
}
int main() {
  f();
}
