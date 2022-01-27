// PR c++/100198
// { dg-do compile { target c++11 } }

template <int... E>
void f() {
  ([] { enum e { e = E }; }(), ...); // { dg-bogus "" "" { xfail *-*-* } }
}

template void f<0>();
