// PR c++/41786

struct A { A(int, char const*); };
int main() {
  int i = 0, *b = &i;
  A a(int(b[i]), "hello");
}
