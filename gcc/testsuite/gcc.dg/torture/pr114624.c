/* { dg-do compile } */

int a, b;
int main() {
  int c, d = 1;
  while (a) {
    while (b)
      if (d)
        while (a)
          ;
    for (; b < 2; b++)
      if (b)
        for (c = 0; c < 8; c++)
          d = 0;
      else
        for (a = 0; a < 2; a++)
          ;
  }
  return 0;
}
