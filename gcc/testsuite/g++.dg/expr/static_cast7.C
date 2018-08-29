// Regression test for bug 39415 (and its duplicate 44916).
struct S {};
struct T : S {};
int f(const T*) { return 0; }
void f(T*);
int main() {
  S* s(0);
  int a = f(static_cast<const T*>(s));
  int b = f(static_cast<const T*>(0));
}
