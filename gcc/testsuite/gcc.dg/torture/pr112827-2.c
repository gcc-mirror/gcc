/* { dg-do compile } */

short a, b[1], f;
char c, g;
int d, e;
int main() {
  for (; f; f++) {
    for (d = 0; d < 2; d++)
      ;
    if (a)
      for (g = 0; g < 2; g++)
        for (c = 0; c < 2; c += b[d+g])
          ;
    for (; e; e++)
      ;
  }
  return 0;
}
