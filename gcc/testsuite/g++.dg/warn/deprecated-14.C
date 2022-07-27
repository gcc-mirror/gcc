// PR c++/90451
// { dg-do compile { target c++11 } }

struct myclass{
  [[deprecated("deprecated-static1")]] static void stat1() { }
  [[deprecated("deprecated-static2")]] static void stat2() { }
  [[deprecated("deprecated-static3")]] static void stat3() { }
  [[deprecated("deprecated-static4")]] static void stat4() { }

  [[deprecated("deprecated-non1")]] void non1() { }
  [[deprecated("deprecated-non2")]] void non2() { }
};

[[deprecated("deprecated-global1")]] void fn1();
[[deprecated("deprecated-global2")]] void fn2();
[[deprecated("deprecated-global3")]] void fn3();

[[deprecated("deprecated-global4")]] void fn4();
[[deprecated("deprecated-global5")]] void fn5();
[[deprecated("deprecated-global6")]] void fn6();
[[deprecated("deprecated-global7")]] void fn7();
[[deprecated("deprecated-global8")]] void fn8();

namespace N
{
  [[deprecated("deprecated-ns1")]] void fn1();
  [[deprecated("deprecated-ns2")]] void fn2();
  [[deprecated("deprecated-ns3")]] void fn3();
}

int main()
{
  myclass::stat1(); // { dg-bogus "deprecated-static1.*deprecated-static1" }
  // { dg-warning "deprecated-static1" "" { target *-*-* } .-1 }
  &myclass::stat2; // { dg-bogus "deprecated-static2.*deprecated-static2" }
  // { dg-warning "deprecated-static2" "" { target *-*-* } .-1 }
  auto x = myclass::stat3; // { dg-bogus "deprecated-static3.*deprecated-static3" }
  // { dg-warning "deprecated-static3" "" { target *-*-* } .-1 }
  (void) myclass::stat4; // { dg-bogus "deprecated-static4.*deprecated-static4" }
  // { dg-warning "deprecated-static4" "" { target *-*-* } .-1 }

  myclass m;
  m.myclass::non1(); // { dg-bogus "deprecated-non1.*deprecated-non1" }
  // { dg-warning "deprecated-non1" "" { target *-*-* } .-1 }
  &myclass::non2; // { dg-bogus "deprecated-non2.*deprecated-non2" }
  // { dg-warning "deprecated-non2" "" { target *-*-* } .-1 }

  fn1(); // { dg-bogus "deprecated-global1.*deprecated-global1" }
  // { dg-warning "deprecated-global1" "" { target *-*-* } .-1 }
  &fn2; // { dg-bogus "deprecated-global2.*deprecated-global2" }
  // { dg-warning "deprecated-global2" "" { target *-*-* } .-1 }
  auto xg = fn3; // { dg-bogus "deprecated-global2.*deprecated-global3" }
  // { dg-warning "deprecated-global3" "" { target *-*-* } .-1 }
  (void) fn7; // { dg-bogus "deprecated-global7.*deprecated-global7" }
  // { dg-warning "deprecated-global7" "" { target *-*-* } .-1 }

  ::fn4(); // { dg-bogus "deprecated-global4.*deprecated-global4" }
  // { dg-warning "deprecated-global4" "" { target *-*-* } .-1 }
  &::fn5; // { dg-bogus "deprecated-global5.*deprecated-global5" }
  // { dg-warning "deprecated-global5" "" { target *-*-* } .-1 }
  auto xgs = ::fn6; // { dg-bogus "deprecated-global2.*deprecated-global6" }
  // { dg-warning "deprecated-global6" "" { target *-*-* } .-1 }
  (void) ::fn8; // { dg-bogus "deprecated-global8.*deprecated-global8" }
  // { dg-warning "deprecated-global8" "" { target *-*-* } .-1 }

  N::fn1(); // { dg-bogus "deprecated-ns1.*deprecated-ns1" }
  // { dg-warning "deprecated-ns1" "" { target *-*-* } .-1 }
  &N::fn2; // { dg-bogus "deprecated-ns2.*deprecated-ns2" }
  // { dg-warning "deprecated-ns2" "" { target *-*-* } .-1 }
  auto xgn = N::fn3; // { dg-bogus "deprecated-ns2.*deprecated-ns3" }
  // { dg-warning "deprecated-ns3" "" { target *-*-* } .-1 }
}
