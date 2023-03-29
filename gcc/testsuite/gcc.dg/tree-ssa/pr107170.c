// { dg-do compile }
// { dg-options "-O2" }

int main() {
  double a;
  if (__builtin_signbit(a))
    __builtin_abort();
}
