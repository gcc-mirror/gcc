// { dg-do compile }

struct b {
  ~b() {}
};
struct c {
  c(long, const int &, b = b());
};
int _setjmp();
long d;
void e(int *);
int a;
int main()
{
  auto f = [](int *data) { e(data); };
  f(&a);
  c(d, _setjmp());
}
