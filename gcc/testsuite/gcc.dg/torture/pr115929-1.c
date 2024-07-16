/* { dg-require-effective-target lp64 } */
/* { dg-options "-fno-gcse -fschedule-insns -fno-guess-branch-probability -fno-tree-fre -fno-tree-ch" } */

int printf(const char *, ...);
int a[6], b, c;
char d, l;
struct {
  char e;
  int f;
  int : 8;
  long g;
  long h;
} i[1][9] = {0};
unsigned j;
void n(char p) { b = b >> 8 ^ a[b ^ p]; }
int main() {
  int k, o;
  while (b) {
    k = 0;
    for (; k < 9; k++) {
      b = b ^ a[l];
      n(j);
      if (o)
        printf(&d);
      long m = i[c][k].f;
      b = b >> 8 ^ a[l];
      n(m >> 32);
      n(m);
      if (o)
        printf("%d", d);
      b = b >> 8 ^ l;
      n(2);
      n(0);
      if (o)
        printf(&d);
      b = b ^ a[l];
      n(i[c][k].g >> 2);
      n(i[c][k].g);
      if (o)
        printf(&d);
      printf("%d", i[c][k].f);
    }
  }
  return 0;
}
