// { dg-do run }
// { dg-options "-O1 -ftree-vrp" }

int a, b = -1, c;
int d = 1;
static inline signed char e(signed char f, int g) { return g ? f : 0; }
static inline signed char h(signed char f) { return f < a ? f : f < a; }
static inline unsigned char i(unsigned char f, int g) { return g ? f : f > g; }
void j() {
L:
  c = e(1, i(h(b), d));
  if (b)
    return;
  goto L;
}
int main() {
  j();
  if (c != 1)
    __builtin_abort ();
  return 0;
}
