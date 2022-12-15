// PR target/107404
// { dg-do run }
// { dg-options "-O3" }

unsigned long long a;
void b(unsigned long long *f, int p2) { *f ^= p2; }
long c;
char e, i;
short g, m;
long long ab[1][25][21][22];
unsigned long long aa[1][21][22];
unsigned long long ae[1][25][21][21];
long long ac[129360];
char ad[25][1][21];
char ah[1][25][1][21];
short af[100];
long max(long f, unsigned long p2) { return f < p2 ? p2 : f; }
const int &max2(const int &f, const int &p2) { return f < p2 ? p2 : f; }
void foo(unsigned f, unsigned p2, char l, char p4, long long n[][25][21][22],
        unsigned long long p6[][21][22], unsigned long long u[][25][21][21]) {
  long an;
  for (int j = 0; j < 4; j = p2)
    for (short k = 0; k < 7; k += 2)
      for (short o = 0; o < (short)p2 + 21742; o = l) {
        for (signed char p = 2; p < 9; p += p4)
          if (p6[j][o][p])
            for (long q(3); 4 ? n[0][k][o][0] : 0;
                 q += p6[0][o][0] ? p6[j][0][p] : 0)
              ac[j + q] = 5066799590;
        for (long r(p4 - 16); r < 21; r += 4) {
          ad[k][o][r] = max(u[j][k][o][r], f + u[j][k][o][r]);
          long d = u[j][k][o][r];
          an = d < p2 ? p2 : d;
          e = ah[j][k][o][r] = an;
          af[o * r] = i;
        }
        for (short s(c); s < (short)p2; s = 2)
          for (short am(m); am; am = max2(3, p2))
            for (long y = 0; y; y = 3)
              for (short t(0); t < max2(g, 0);)
                ;
      }
}
int main() {
  foo(7, 1558227751, 104, 16, ab, aa, ae);
  for (unsigned long v = 0; v < 5; ++v)
    for (unsigned long w = 0; w < 1; ++w)
      for (unsigned long x = 0; x < 21; ++x)
        b(&a, ad[v][w][x]);

  if (a)
    __builtin_abort();
}
