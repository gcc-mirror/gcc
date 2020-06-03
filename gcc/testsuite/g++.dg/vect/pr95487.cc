// { dg-do compile }
// { dg-additional-options "-O3" }
// { dg-additional-options "-march=skylake-avx512" { target x86_64-*-* i?86-*-* } }

int a;
bool d;
char e;
extern short f[];
extern int g[];
short j;
void h() {
  for (short b = j; b < 0; b += 2) {
    f[b] = 0;
    if (d) {
      for (char c = 0; c < a; c += 3)
        e = 0;
      g[b] = 0;
    }
  }
}
