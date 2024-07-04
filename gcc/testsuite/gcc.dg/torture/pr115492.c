/* { dg-do run } */

int a = 2, b=0, *c = &a, *d = &a, e=0;
[[gnu::noipa]]
void f(int) {}
[[gnu::noipa]]
int h(int *k) {
  int ***j;
  if (b) {
    *j = &k; // Note the unintialized j is used here
             // but since it is conditional and b is always zero, there should no
             // effect otherwise.
    ***j;
  }
  f(*k);
  *d = e;
  return *k;
}
int main() { if (h(c)) __builtin_abort(); }
