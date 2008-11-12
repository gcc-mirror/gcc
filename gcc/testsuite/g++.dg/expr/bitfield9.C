// PR c++/32346
// { dg-do run }
// { dg-options "-Wno-overflow" }

extern "C" void abort();

struct S {
  long long i : 32;
};

void f(int i, int j) {
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
