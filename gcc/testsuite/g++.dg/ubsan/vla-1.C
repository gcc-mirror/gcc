// { dg-do run }
// { dg-options "-Wno-vla -fsanitize=undefined" }
// { dg-output "index 1 out of bounds" }

void f(int i) {
  int ar[i] = { 42, 24 };
}

int main()
{
  f(1);
}
