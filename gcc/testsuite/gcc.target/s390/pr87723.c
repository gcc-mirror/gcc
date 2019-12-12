/* { dg-do compile } */
/* { dg-options "-O3 -march=z196 -m64 -mzarch" } */

unsigned long a;
int b;
void c(char* i) {
  for (;;) {
    char g = 0;
    for (; g < 24; ++g)
      b = a << g | a >> 64 - g;
    {
      char *d = i;
      long h = b;
      char e = 0;
      for (; e < 8; ++e)
	d[e] = h;
    }
    char *d = i;
    signed e;
    unsigned long f = 0;
    e = 7;
    for (; e; --e) {
      f <<= 8;
      f |= d[e];
    }
    for (; e < 8; ++e)
      d[e] = f;
  }
}
