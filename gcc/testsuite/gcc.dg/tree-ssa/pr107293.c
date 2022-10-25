// { dg-do run }
// { dg-options "-w -Os" }

short a;
int b[1];

int c(int p) {
  return (p < 0) ? 0 : 10 + ((p / 100 - 16) / 4);
}

void f(int n) {
  while (1) {
    int m = n;
    while ((m ) )
      m /= 2;
    break;
  }
}

void g() {
  int h = a = 0;
  for (; h + a <= 0; a++) {
    if (b[c(a - 6)])
      break;
    f(a);
  }
}
int main() {
  g();
  if (a != 1)
    __builtin_abort ();
}
