/* { dg-do run } */
/* { dg-options "-O3" } */

struct {
  signed a;
} b;
int a, f, j, l;
char c, k, g, e;
short d[2] = {0};
int *i = &j;

volatile int glob;
void __attribute__((noipa)) sth (const char *, int a)
{
  glob = a;
  return;
}

void marker_37() {
  a++;
  sth ("%d\n", a);
}
unsigned long long m(unsigned, char, unsigned, short);
int n(int, unsigned char, long long);
int o(long long, unsigned, unsigned);
unsigned short p(void) {
  int *r = &l;
  *r |= ({
    long long y = (m(c, c, 0, c), b.a);
    y;
  });
  return 0;
}
unsigned long long m(unsigned q, char v, unsigned s, short u) {
  unsigned short ab = 5;
  if (n(q, ab, d[1]))
    for (; g; g++)
      ;
  return c;
}
int n(int af, unsigned char e, long long ae) {
  unsigned ag = 4;
  int *ah = &f;
  *ah = ({ short ad = o(af, f, ag); ad<0 || ad> e; });
  return *i;
}
int o(long long aj, unsigned ai, unsigned ak) {
  for (; e; e--) {
    int *al = &f;
    for (; k; k++)
      *al = 0;
  }
  if (18446744073709551606UL != (unsigned long long) aj)
    ;
  else
    marker_37();
  return ak;
}
int f123() {
  c = 0xf6;
  p();
  return 0;
}
int main() {
  return f123();
}
