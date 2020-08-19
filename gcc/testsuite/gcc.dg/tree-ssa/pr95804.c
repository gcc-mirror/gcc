/* PR tree-optimization/95804 */
/* { dg-do compile } */
/* { dg-options "-O3" } */

int a, b, c, d, e, f;
void g() {
  short *h = (short*)&d;
  char *i = (char*)&b;
  for (; e; e++) {
    for (; f; f++) {
      b = 3;
      if ((c = 8) >= *i)
        a = 5 ? *h : 0;
      h = (short*)g;
    }
    i = (char*)&c;
  }
}
