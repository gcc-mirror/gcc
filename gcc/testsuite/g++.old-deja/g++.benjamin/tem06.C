// 980945 bkoz
// test for correct operators at link time

/*
/tmp/cca211431.o: In function `void blah<foo<int> >(foo<int> const &)':
/tmp/cca211431.o(.void gnu.linkonce.t.blah<foo<int> >(foo<int> const &)+0x1e): undefined reference to `void x<int>(int const &)'
*/

template<class T>
class foo {
public:
  foo () {}
  friend void x (const T &) { }
};

template<class T>
void blah (const T &) {
  T y;
  x (4);
};

int main () {
  const foo<int> v;
  blah (v);
}

/*
fno-exceptions -fno-rtti

1.98r1.o:
00000000 W __t3foo1Zi
00000000 W blah__H1Zt3foo1Zi_RCX01_v
00000000 t gcc2_compiled.
00000000 T main
         U x__H1Zi_RCX01_v

1.egcs.o:
00000000 W __t3foo1Zi
00000000 W blah__H1Zt3foo1Zi_RCX01_v
00000000 t gcc2_compiled.
00000000 T main
00000000 W x__FRCi


the reason this goes away at -O is because the U or W function is
elided completely.  

*/

