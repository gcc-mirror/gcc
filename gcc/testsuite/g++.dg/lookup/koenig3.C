extern "C" void abort ();

struct S {
};
void f(S, int) { abort(); }
void f(S, double) {}

S s;

int main() {
  extern void f(S, int);
  f(s, 3.0);
}
