struct a {
  int b;
  long c;
  long d;
  bool f;
};
struct g {
  int b;
  long c;
  long d;
  bool : 1;
} h;
struct l {
  union i {
    a j;
    g k;
    i(g m) : k(m) {}
  } data;
};
int main() {
  l e{g()};
  h = e.data.k;
  return 0;
}
