// PR c++/32346
// { dg-do run }
// { dg-options "-Wno-overflow" }

extern "C" void abort();

typedef int int32_t __attribute__((mode (__SI__)));

struct S {
  long long i : 32;
};

void f(int32_t i, int32_t j) {
  if (i != 0xabcdef01)
    abort();
  if (j != 0)
    abort();
}

void g(S s) {
  f(s.i, 0);
}

int main() {
  S s;
  s.i = 0xabcdef01;
  g(s);
}
