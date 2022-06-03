/* { dg-do run } */

int a, b, c[1], d[2], *e = c;
int main() {
  int f = 0;
  for (; b < 2; b++) {
    int g;
    if (f)
      g++, b = 40;
    a = d[b * b];
    for (f = 0; f < 3; f++) {
      if (e)
        break;
      g--;
      if (a)
        a = g;
    }
  }
  return 0;
}
