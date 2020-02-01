// PR c++/86216
// { dg-do compile { target c++11 } }
// { dg-additional-options -Wno-vla }

template <typename T> void b(int n, T arg) {
  int buffer[arg];
  int buffer2[arg][arg];
  [&] {
    n = sizeof(buffer);
    n = sizeof(buffer2);	// { dg-bogus "sorry" "" { xfail *-*-* } }
  }();
}
int main() { b(2, 3); }
