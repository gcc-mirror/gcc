// { dg-options "-frepo" }

extern "C" inline void f() {}

int main () {
  f();
}
