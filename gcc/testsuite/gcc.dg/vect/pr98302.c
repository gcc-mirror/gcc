#include "tree-vect.h"

int c = 1705;
char a;
long f = 50887638;
unsigned long long *h(unsigned long long *k, unsigned long long *l) {
  return *k ? k : l;
}
void aa() {}
int main() {
  check_vect ();

  long d = f;
  for (char g = 0; g < (char)c - 10; g += 2) {
    unsigned long long i = d, j = 4;
    a = *h(&i, &j) << ((d ? 169392992 : 0) - 169392955LL);
  }
  if (a)
    __builtin_abort();

  return 0;
}
