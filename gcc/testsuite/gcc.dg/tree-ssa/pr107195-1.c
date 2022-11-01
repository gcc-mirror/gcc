// { dg-do run }
// { dg-options "-O1 -fno-tree-ccp" }

int a, b;
int main() {
  int c = 0;
  if (a)
    c = 1;
  c = 1 & (a && c) && b;
  if (a) {
    b = c;
    __builtin_abort ();
  }
  return 0;
}
