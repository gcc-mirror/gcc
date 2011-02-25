// PR c++/41786
// { dg-options -std=c++0x }

struct A { A(int, char const*); };
int main() {
  int i = 0, *b = &i;
  A a(int(b[i]), "hello");
}
