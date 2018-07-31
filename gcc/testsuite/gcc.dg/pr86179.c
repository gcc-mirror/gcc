/* { dg-do compile } */
/* { dg-options "-O3" } */

void c(int *d, char *g)
{
  char *a, *b, *e;
  int f;
  for (; f; f -= 8) {
      *d++ = *e++ | (unsigned)*g++ << 8 | (unsigned)*b++ << 16 |
	  (unsigned)*a++ << 24;
      *d++ = *e++ | (unsigned)*g++ << 8 | (unsigned)*b++ << 16 |
	  (unsigned)*a++ << 24;
  }
}
