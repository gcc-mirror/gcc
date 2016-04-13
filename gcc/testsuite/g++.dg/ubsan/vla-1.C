// { dg-do run }
// Disable exceptions to prevent the erroneous initializer from
// throwing before the sanitizer instrumentation has detected
// the problem.
// { dg-options "-Wno-vla -fno-exceptions -fsanitize=undefined" }
// { dg-output "index 1 out of bounds" }

void f(int i) {
  int ar[i] = { 42, 24 };
}

int main()
{
  f(1);
}
