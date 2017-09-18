// { dg-options "-fsanitize=address -O2 -g -Wno-write-strings" }

class a {
  struct b {
    b(int, int);
  } c;

public:
  int d;
  a(char *) : c(0, d) {}
};
class e {
  int f(const int &, const int &, const int &, bool, bool, bool, int, bool);
};
class g {
public:
  static g *h();
  void i(a, void *);
};
int e::f(const int &, const int &, const int &, bool j, bool, bool, int, bool) {
  g::h()->i("", &j);
}
