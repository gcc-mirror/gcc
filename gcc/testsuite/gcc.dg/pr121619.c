/* { dg-do run } */
/* { dg-options "-O3 -fno-gcse -fno-tree-ter -fno-guess-branch-probability -fno-forward-propagate" } */

int printf(const char *, ...);
long a, c, d;
char b;
int main() {
f : {
  short g = 100;
  int h = 1;
  while (1) {
    char i = 0;
    if (a)
      i = h = -b;
    short j = g;
    c = h ^ g;
    g = ~(-h / c + 1);
    if (b > 6) {
      a = g && -1;
      goto f;
    }
    if (j < 100)
      printf("%ld\n", d);
    if (g - 1)
      break;
    b = i;
  }
  int k = 2L % g;
  if (k)
    goto f;
  }
  return 0;
}
