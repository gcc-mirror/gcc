// PR c++/21089
// { dg-do run }

extern "C" void abort();

static const double a = 1.0;
struct S {
  S();
};
static S s;
static const double b = a + 1.0;

S::S() {
  if (b < 1.9 || b > 2.1)
    abort ();
}

int main () {
}
