extern "C" void abort ();

struct A {
  union {
    int a;
    double b;
    int d;
  };
  int c;
};

struct B : public A {
  union {
    double a;
    void *c;
  };
  float b;
  int e;
};

int main ()
{
  struct B b;

  b.a = 1.5;
  b.b = 2.5;
  b.d = 1;
  b.e = 2;
  if (b.a != 1.5 || b.b != 2.5 || b.d != 1 || b.e != 2)
    abort ();
  b.c = &b.a;
  b.d = b.e;
  if (b.c != &b.a || b.d != 2)
    abort ();
  return 0;
}
