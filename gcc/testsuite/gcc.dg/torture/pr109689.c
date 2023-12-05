/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

int a, b, c, d, e;
int main() {
  char f;
  while (a) {
    int g, h = 3;
    if (b)
    i:
      if (d)
        goto j;
  k:
    if (a) {
    j:
      if (!g)
        goto k;
      if (e) {
        while (e)
          e = f;
        h = 0;
        goto i;
      }
      if (!h)
        for (; g < 1; g++)
          ;
      g = ~((~c & h & c) ^ ~g);
      if (!g)
        for (; a < 1; a++)
          f++;
    }
  }
  return 0;
}
