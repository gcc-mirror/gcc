/* { dg-do run } */

int f(_Bool t)
{
        int tt = t;
        unsigned x = -1;
        int xx = x;
        return xx <= tt;
}

int a, b;
void c() {}
__attribute__((noipa))
void h() {__builtin_abort();}
int d() {
  unsigned f[1];
  int i;
  if (a)
    goto h;
  f[0] = -1;
  while (1) {
    c();
    for (; a < 1; a++) {
      if (0) {
      j:
        continue;
      }
      i = f[0];
      if (a)
        break;
      b = i >= (b == 0);
    }
    if (!b) {
      if (0) {
      h:
        goto j;
      }
      return 0;
    }
    h();
  }
}
int main() {
  d();
  return 0;
}
