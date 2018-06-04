// { dg-additional-options -std=c++17 }

template<int ... Ns> int f() { return (Ns + ...); }
template<int N> int g() {
  return f<__integer_pack(N)...>(); // Fine.
}
template<int N> int h() {
  return f<(2*__integer_pack(N))...>(); // { dg-bogus "sorry" "" { xfail *-*-* } }
}
int main() { return g<3>()+h<3>(); }
