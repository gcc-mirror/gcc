/* { dg-do compile } */
/* { dg-options "-O1 -ftree-vrp -fno-tree-ccp -fno-tree-forwprop" } */
int a, b, d;
unsigned c;
int main() {
  char e = -10;
  int f = 1, g = 0;
  if (a) {
    char h = e;
  i:
    c = ~h - (-g & f || e);
    int j = b % c;
    g = j % 9;
    if (c) {
      if (d)
        e = 0;
      while (!g)
        ;
      int k = 0;
    l:
      if (k)
        goto i;
    }
  }
  if (e > -10) {
    if (g)
      f = 0;
    goto l;
  }
  return 0;
}
