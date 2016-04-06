struct __attribute__((abi_tag("a"))) X { };
template<typename T> struct Y { X f() { return X(); } };
template struct Y<int>;
// { dg-final { scan-assembler "_ZN1YIiE1fB1aEv" } }
