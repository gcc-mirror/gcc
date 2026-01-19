// { dg-do compile }

void _setjmp();
void a() __attribute__((__noreturn__));
struct b {
  b(int c) {
    if (c)
      a();
  }
  ~b();
};
int d;
void l(long);
void e() {
  b a(d);
  for (int f = 0; f < 10 ; ++f) {
    l(f - 1);
    _setjmp();
  }
}
