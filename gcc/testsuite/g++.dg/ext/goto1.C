// PR c++/79781
// { dg-do compile { target int128 } }
// { dg-options "" }

void c() {
  static __int128_t d = (long)&&a - (long)&&b;
a:
b:;
}
